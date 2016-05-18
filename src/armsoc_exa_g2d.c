/* -*- mode: C; c-file-style: "k&r"; tab-width 4; indent-tabs-mode: t; -*- */

/*
 * Copyright (C) 2015 - Tobias Jakobi
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "armsoc_driver.h"
#include "armsoc_exa.h"

#include "exa.h"

#include <libdrm/exynos_drmif.h>
#include <exynos/exynos_fimg2d.h>

#define EXA_G2D_DEBUG 0

#if defined(EXA_G2D_DEBUG) && (EXA_G2D_DEBUG == 1)
#define EXA_G2D_DEBUG_SOLID
#define EXA_G2D_DEBUG_COPY
#define EXA_G2D_DEBUG_PERF
#endif

/*
 * EXA implementation using the Exynos G2D block to accelerate drawing
 * operations (solid fill, copy, etc.).
 */

enum e_g2d_exa_constants {
	g2d_exa_solid_batch = 8,
	g2d_exa_copy_batch = 8,
};

enum e_g2d_exa_flags {
	g2d_exa_copy_move = (1 << 0),
};

enum e_g2d_exa_operation {
	g2d_exa_op_unset,
	g2d_exa_op_solid,
	g2d_exa_op_copy,
};

struct G2DStats {
	unsigned long solid_ops[2];
	unsigned long copy_ops[2];
};

struct SolidG2DOp {
	PixmapPtr p;

	struct g2d_image dst;
	struct g2d_rect rects[g2d_exa_solid_batch];
	unsigned int num_rects;
};

struct CopyG2DOp {
	PixmapPtr p;
	unsigned int flags;

	struct g2d_image src;
	struct g2d_image dst;

	struct g2d_rect src_rects[g2d_exa_copy_batch];
	struct g2d_rect dst_rects[g2d_exa_copy_batch];
	unsigned int num_rects;
};

struct ExynosG2DRec {
	struct ARMSOCEXARec base;
	ExaDriverPtr exa;
	struct g2d_context *g2d_ctx;
	struct G2DStats stats;

	enum e_g2d_exa_operation current_op;
	void *op_data;
};

/*
 * EXA API documentation:
 * http://cgit.freedesktop.org/xorg/xserver/tree/exa/exa.h
 */

static void perf_record(struct G2DStats *stats, enum e_g2d_exa_operation op,
	unsigned int accel)
{
#if defined(EXA_G2D_DEBUG_PERF)
	unsigned long *ops;

	switch (op) {
	case g2d_exa_op_solid:
		ops = stats->solid_ops;
		break;

	case g2d_exa_op_copy:
		ops = stats->copy_ops;
		break;

	case g2d_exa_op_unset:
	default:
		assert(0);
		return;
	}

	if (accel)
		ops[0]++;
	else
		ops[1]++;
#endif
}

static void perf_stats(const struct G2DStats *stats)
{
#if defined(EXA_G2D_DEBUG_PERF)
	EARLY_INFO_MSG("PERF: EXA solid: accel = %lu, nonaccel = %lu",
		stats->solid_ops[0], stats->solid_ops[1]);

	EARLY_INFO_MSG("PERF: EXA copy: accel = %lu, nonaccel = %lu",
		stats->copy_ops[0], stats->copy_ops[1]);
#endif
}

static struct ExynosG2DRec*
G2DPrivFromPixmap(PixmapPtr pPixmap)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pPixmap->drawable.pScreen);
	struct ARMSOCRec *pARMSOC = ARMSOCPTR(pScrn);
	return (struct ExynosG2DRec*)(pARMSOC->pARMSOCEXA);
}

#if defined(EXA_G2D_DEBUG_SOLID) || defined(EXA_G2D_DEBUG_COPY)
static const char*
translate_gxop(unsigned int op)
{
	switch (op) {
	case GXclear:				/* 0x0: 0 */
		return "clear";
	case GXand:					/* 0x1: src AND dst */
		return "and";
	case GXandReverse:			/* 0x2: src AND NOT dst */
		return "and/reverse";
	case GXcopy:				/* 0x3: src */
		return "copy";
	case GXandInverted:			/* 0x4: NOT src AND dst */
		return "and/inverted";
	case GXnoop:				/* 0x5: dst */
		return "noop";
	case GXxor:					/* 0x6: src XOR dst */
		return "xor";
	case GXor:					/* 0x7: src OR dst */
		return "or";
	case GXnor:					/* 0x8: NOT src AND NOT dst */
		return "nor";
	case GXequiv:				/* 0x9: NOT src XOR dst */
		return "equiv";
	case GXinvert:				/* 0xa: NOT dst */
		return "invert";
	case GXorReverse:			/* 0xb: src OR NOT dst */
		return "or/reverse";
	case GXcopyInverted:		/* 0xc: NOT src */
		return "copy/inverted";
	case GXorInverted:			/* 0xd: NOT src OR dst */
		return "or/inverted";
	case GXnand:				/* 0xe: NOT src OR NOT dst */
		return "nand";
	case GXset:					/* 0xf: 1 */
		return "set";
	default:
		return "unknown GX operations";
	}
}
#endif

static unsigned int
translate_pixmap_depth(PixmapPtr pPixmap)
{
	switch (pPixmap->drawable.depth) {
    case 32:
        return G2D_COLOR_FMT_ARGB8888 | G2D_ORDER_AXRGB;

    case 24:
        return G2D_COLOR_FMT_XRGB8888 | G2D_ORDER_AXRGB;

    case 16:
        return G2D_COLOR_FMT_RGB565 | G2D_ORDER_AXRGB;

    case 8:
        return G2D_COLOR_FMT_A8 | G2D_ORDER_AXRGB;

    case 1:
        return G2D_COLOR_FMT_A1 | G2D_ORDER_AXRGB;

    default:
		assert(0);
		return 0;
    }
}

static Bool
PrepareSolidG2D(PixmapPtr pPixmap, int alu, Pixel planemask, Pixel fg)
{
	struct ARMSOCPixmapPrivRec *pixPriv = exaGetPixmapDriverPrivate(pPixmap);
	struct ExynosG2DRec *g2dPriv = G2DPrivFromPixmap(pPixmap);
	struct SolidG2DOp *solidOp;

#if defined(EXA_G2D_DEBUG_SOLID)
	EARLY_INFO_MSG("DEBUG: PrepareSolidG2D: pixmap = %p, alu = %s, "
		"planemask = 0x%x, accel = %u",
		pPixmap, translate_gxop(alu), (unsigned int)planemask,
		!!is_accel_pixmap(pixPriv));
#endif

	if (!is_accel_pixmap(pixPriv))
		goto fail;

	if (alu != GXcopy)
		goto fail;

	if (planemask != 0xffffffff)
		goto fail;

	perf_record(&g2dPriv->stats, g2d_exa_op_solid, 1);

	assert(g2dPriv->current_op == g2d_exa_op_unset);

	solidOp = calloc(1, sizeof(struct SolidG2DOp));

	solidOp->p = pPixmap;

	solidOp->dst.color_mode = translate_pixmap_depth(pPixmap);
	solidOp->dst.width = pPixmap->drawable.width;
	solidOp->dst.height = pPixmap->drawable.height;
	solidOp->dst.stride = exaGetPixmapPitch(pPixmap);
	solidOp->dst.color = fg;
	solidOp->dst.buf_type = G2D_IMGBUF_GEM;
	solidOp->dst.bo[0] = armsoc_bo_handle(pixPriv->bo);

	g2dPriv->current_op = g2d_exa_op_solid;
	g2dPriv->op_data = solidOp;

	return TRUE;

fail:
	perf_record(&g2dPriv->stats, g2d_exa_op_solid, 0);

	return FALSE;
}

static void
SolidG2D(PixmapPtr pPixmap, int x1, int y1, int x2, int y2)
{
	struct ExynosG2DRec *g2dPriv = G2DPrivFromPixmap(pPixmap);
	struct SolidG2DOp *solidOp;
	struct g2d_rect *rect;

#if defined(EXA_G2D_DEBUG_SOLID)
	EARLY_INFO_MSG("DEBUG: Solid2D: pixmap = %p, "
		"x1 = %d, y1 = %d, x2 = %d, y2 = %d", pPixmap,
		x1, y1, x2, y2);
#endif

	assert(g2dPriv->current_op == g2d_exa_op_solid);

	solidOp = g2dPriv->op_data;
	
	if (solidOp->num_rects == g2d_exa_solid_batch) {
		// TODO: error handling
		g2d_solid_fill_multi(g2dPriv->g2d_ctx, &solidOp->dst, solidOp->rects, g2d_exa_solid_batch);

		solidOp->num_rects = 0;
	}

	rect = &solidOp->rects[solidOp->num_rects];

	assert(pPixmap == solidOp->p);

	rect->x = x1;
	rect->y = y1;
	rect->w = x2 - x1;
	rect->h = y2 - y1;

	solidOp->num_rects++;
}

static void
DoneSolidG2D(PixmapPtr pPixmap)
{
	struct ExynosG2DRec *g2dPriv = G2DPrivFromPixmap(pPixmap);
	struct SolidG2DOp *solidOp;

#if defined(EXA_G2D_DEBUG_SOLID)
	EARLY_INFO_MSG("DEBUG: DoneSolidG2D: pixmap = %p", pPixmap);
#endif

	assert(g2dPriv->current_op == g2d_exa_op_solid);

	solidOp = g2dPriv->op_data;

	if (solidOp->num_rects == 0)
		goto out;

	// TODO: error handling
	g2d_solid_fill_multi(g2dPriv->g2d_ctx, &solidOp->dst, solidOp->rects, solidOp->num_rects);

out:
	g2d_exec(g2dPriv->g2d_ctx);

	free(solidOp);

	g2dPriv->current_op = g2d_exa_op_unset;
	g2dPriv->op_data = NULL;
}

static Bool
PrepareCopyG2D(PixmapPtr pSrc, PixmapPtr pDst, int xdir, int ydir,
		int alu, Pixel planemask)
{
	struct ARMSOCPixmapPrivRec *privSrc = exaGetPixmapDriverPrivate(pSrc);
	struct ARMSOCPixmapPrivRec *privDst = exaGetPixmapDriverPrivate(pDst);
	struct ExynosG2DRec *g2dPriv = G2DPrivFromPixmap(pSrc);
	struct CopyG2DOp *copyOp;

#if defined(EXA_G2D_DEBUG_COPY)
	EARLY_INFO_MSG("DEBUG: PrepareCopyG2D: src = %p, dst = %p, alu = %s, "
		"planemask = 0x%x, src_accel = %u, dst_accel = %u", pSrc, pDst,
		translate_gxop(alu), (unsigned int)planemask,
		!!is_accel_pixmap(privSrc), !!is_accel_pixmap(privDst));
#endif

	if (!is_accel_pixmap(privSrc))
		goto fail;

	if (!is_accel_pixmap(privDst))
		goto fail;

	if (alu != GXcopy)
		goto fail;

	if (planemask != 0xffffffff)
		goto fail;

	perf_record(&g2dPriv->stats, g2d_exa_op_copy, 1);

	assert(g2dPriv->current_op == g2d_exa_op_unset);

	copyOp = calloc(1, sizeof(struct CopyG2DOp));

	copyOp->p = pDst;

	copyOp->src.color_mode = translate_pixmap_depth(pSrc);
	copyOp->src.width = pSrc->drawable.width;
	copyOp->src.height = pSrc->drawable.height;
	copyOp->src.stride = exaGetPixmapPitch(pSrc);
	copyOp->src.buf_type = G2D_IMGBUF_GEM;
	copyOp->src.bo[0] = armsoc_bo_handle(privSrc->bo);

	/*
	 * If this is a move operation (source == destination pixmap),
	 * then we can skip setup of the dst G2D image.
	 */
	if (pSrc == pDst) {
		copyOp->flags |= g2d_exa_copy_move;
		goto out;
	}

	copyOp->dst.color_mode = translate_pixmap_depth(pDst);
	copyOp->dst.width = pDst->drawable.width;
	copyOp->dst.height = pDst->drawable.height;
	copyOp->dst.stride = exaGetPixmapPitch(pDst);
	copyOp->dst.buf_type = G2D_IMGBUF_GEM;
	copyOp->dst.bo[0] = armsoc_bo_handle(privDst->bo);

out:
	g2dPriv->current_op = g2d_exa_op_copy;
	g2dPriv->op_data = copyOp;

	return TRUE;

fail:
	perf_record(&g2dPriv->stats, g2d_exa_op_copy, 0);

	return FALSE;
}

static void
CopyG2D(PixmapPtr pDstPixmap, int srcX, int srcY, int dstX, int dstY,
		int width, int height)
{
	struct ExynosG2DRec *g2dPriv = G2DPrivFromPixmap(pDstPixmap);
	struct CopyG2DOp *copyOp;
	struct g2d_rect *src_rect, *dst_rect;

#if defined(EXA_G2D_DEBUG_COPY)
	EARLY_INFO_MSG("DEBUG: CopyG2D: dst = %p, src_x = %d, src_y = %d, "
		"dst_x = %d, dst_y = %d, w = %d, h = %d", pDstPixmap,
		srcX, srcY, dstX, dstY, width, height);
#endif

	assert(g2dPriv->current_op == g2d_exa_op_copy);

	copyOp = g2dPriv->op_data;

	if (copyOp->num_rects == g2d_exa_copy_batch) {
		// TODO: error handling
		if (copyOp->flags & g2d_exa_copy_move)
			g2d_move_multi(g2dPriv->g2d_ctx, &copyOp->src,
				copyOp->src_rects, copyOp->dst_rects, g2d_exa_copy_batch);
		else
			g2d_copy_multi(g2dPriv->g2d_ctx, &copyOp->src, &copyOp->dst,
				copyOp->src_rects, copyOp->dst_rects, g2d_exa_copy_batch);

		copyOp->num_rects = 0;
	}

	src_rect = &copyOp->src_rects[copyOp->num_rects];
	dst_rect = &copyOp->dst_rects[copyOp->num_rects];

	assert(pDstPixmap == copyOp->p);

	src_rect->x = srcX;
	src_rect->y = srcY;
	src_rect->w = width;
	src_rect->h = height;

	dst_rect->x = dstX;
	dst_rect->y = dstY;

	copyOp->num_rects++;
}

static void
DoneCopyG2D(PixmapPtr pDstPixmap)
{
	struct ExynosG2DRec *g2dPriv = G2DPrivFromPixmap(pDstPixmap);
	struct CopyG2DOp *copyOp;

#if defined(EXA_G2D_DEBUG_COPY)
	EARLY_INFO_MSG("DEBUG: DoneCopyG2D: dst = %p", pDstPixmap);
#endif

	assert(g2dPriv->current_op == g2d_exa_op_copy);

	copyOp = g2dPriv->op_data;

	if (copyOp->num_rects == 0)
		goto out;

	// TODO: error handling
	if (copyOp->flags & g2d_exa_copy_move)
		g2d_move_multi(g2dPriv->g2d_ctx, &copyOp->src,
			copyOp->src_rects, copyOp->dst_rects, copyOp->num_rects);
	else
		g2d_copy_multi(g2dPriv->g2d_ctx, &copyOp->src, &copyOp->dst,
			copyOp->src_rects, copyOp->dst_rects, copyOp->num_rects);

out:
	g2d_exec(g2dPriv->g2d_ctx);

	free(copyOp);

	g2dPriv->current_op = g2d_exa_op_unset;
	g2dPriv->op_data = NULL;
}

static Bool
CheckCompositeFail(int op, PicturePtr pSrcPicture, PicturePtr pMaskPicture,
		PicturePtr pDstPicture)
{
	return FALSE;
}

static Bool
PrepareCompositeFail(int op, PicturePtr pSrcPicture, PicturePtr pMaskPicture,
		PicturePtr pDstPicture, PixmapPtr pSrc,
		PixmapPtr pMask, PixmapPtr pDst)
{
	return FALSE;
}

/**
 * CloseScreen() is called at the end of each server generation and
 * cleans up everything initialised in InitNullEXA()
 */
static Bool
CloseScreen(CLOSE_SCREEN_ARGS_DECL)
{
	ScrnInfoPtr pScrn = xf86ScreenToScrn(pScreen);
	struct ARMSOCRec *pARMSOC = ARMSOCPTR(pScrn);
	struct ExynosG2DRec *pExynosG2D = (struct ExynosG2DRec *)pARMSOC->pARMSOCEXA;

	perf_stats(&pExynosG2D->stats);

	g2d_fini(pExynosG2D->g2d_ctx);
	exaDriverFini(pScreen);
	free(pExynosG2D->exa);
	free(pExynosG2D);
	pARMSOC->pARMSOCEXA = NULL;

	return TRUE;
}

/*
 * FreeScreen() is called on an error during PreInit and
 * should clean up anything initialised before InitNullEXA()
 * (which currently is nothing)
 */
static void
FreeScreen(FREE_SCREEN_ARGS_DECL)
{
}

struct ARMSOCEXARec*
InitExynosG2DEXA(ScreenPtr pScreen, ScrnInfoPtr pScrn, int fd)
{
	struct ExynosG2DRec *pExynosG2D;
	struct ARMSOCEXARec *pARMSOCEXA;
	ExaDriverPtr exa;
	struct g2d_context *g2d_ctx;

	INFO_MSG("Exynos/G2D EXA mode");

	pExynosG2D = calloc(1, sizeof(struct ExynosG2DRec));
	if (!pExynosG2D)
		goto out;

	pARMSOCEXA = (struct ARMSOCEXARec *)pExynosG2D;

	exa = exaDriverAlloc();
	if (!exa)
		goto free_exynos_exa;

	pExynosG2D->exa = exa;

	g2d_ctx = g2d_init(fd);
	if (!g2d_ctx)
		goto free_exa;

	pExynosG2D->g2d_ctx = g2d_ctx;

	exa->exa_major = EXA_VERSION_MAJOR;
	exa->exa_minor = EXA_VERSION_MINOR;

	exa->pixmapOffsetAlign = 0;
	exa->pixmapPitchAlign = 32;
	exa->flags = EXA_OFFSCREEN_PIXMAPS | EXA_HANDLES_PIXMAPS |
		EXA_SUPPORTS_PREPARE_AUX;
	exa->maxX = 4096;
	exa->maxY = 4096;

	/* Required EXA functions: */
	exa->WaitMarker = ARMSOCWaitMarker;
	exa->CreatePixmap2 = ARMSOCCreatePixmap2;
	exa->DestroyPixmap = ARMSOCDestroyPixmap;
	exa->ModifyPixmapHeader = ARMSOCModifyPixmapHeader;

	exa->PrepareAccess = ARMSOCPrepareAccess;
	exa->FinishAccess = ARMSOCFinishAccess;
	exa->PixmapIsOffscreen = ARMSOCPixmapIsOffscreen;

	/* Accelerate copy and solid fill calls with the G2D. */
	exa->PrepareCopy = PrepareCopyG2D;
	exa->Copy = CopyG2D;
	exa->DoneCopy = DoneCopyG2D;
	exa->PrepareSolid = PrepareSolidG2D;
	exa->Solid = SolidG2D;
	exa->DoneSolid = DoneSolidG2D;

	/* Always fallback for software operations for composite for now. */
	exa->CheckComposite = CheckCompositeFail;
	exa->PrepareComposite = PrepareCompositeFail;

	if (!exaDriverInit(pScreen, exa)) {
		ERROR_MSG("exaDriverInit failed");
		goto free_exa;
	}

	pARMSOCEXA->CloseScreen = CloseScreen;
	pARMSOCEXA->FreeScreen = FreeScreen;

	return pARMSOCEXA;

free_exa:
	free(exa);

free_exynos_exa:
	free(pARMSOCEXA);

out:
	return NULL;
}
