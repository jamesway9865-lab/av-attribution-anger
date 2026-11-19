# =============================================================================
# Step 3: Figures 1-5 (base R graphics).
#
# Input : output/tables/*.csv (from 02_results.R)
# Output: output/figures/Fig1-Fig5 as 300-dpi PNG and vector PDF (cairo devices; fonts embedded)
# Colours: Study 1 HV blue / AV orange; Study 2 ability information green / rule-compliance information
# purple; decomposition: trait purple / momentary yellow / situational green / direct grey. Every series
# also has its own marker shape, so the figures do not rely on colour alone.
# Run   : Rscript code/03_figures.R
# =============================================================================
if (!exists("ROOT")) {
  a <- commandArgs(trailingOnly = FALSE); f <- sub("^--file=", "", a[grepl("^--file=", a)])
  ROOT <- if (nzchar(Sys.getenv("REPO_ROOT"))) Sys.getenv("REPO_ROOT") else
    if (length(f)) normalizePath(file.path(dirname(f[1]), ".."), winslash = "/") else getwd()
}
source(file.path(ROOT, "code", "functions.R"), encoding = "UTF-8")
dir.create(DIR_FIG, recursive = TRUE, showWarnings = FALSE)
rt <- function(n) rd_csv(file.path(DIR_TAB, paste0(n, ".csv")))

INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e4e3df"
HVC <- "#2a78d6"; AVC <- "#eb6834"; ABI <- "#1baf7a"; RUL <- "#6250d6"
C_TR <- "#6250d6"; C_MO <- "#eda100"; C_SI <- "#1baf7a"; C_DI <- "#a3a29c"
LW <- function(pt) pt / 0.75                 # line width in points -> R lwd units
CX <- function(pt) pt / 8                    # font size in points (base size 8 pt)

# number as printed in the figures: round once, half away from zero; |x| < .005 keeps 3 decimals
fm <- function(x) fmt(x, if (x != 0 && abs(x) < 0.005) 3 else 2)
fmci <- function(e, l, h) sprintf("%s [%s, %s]", fm(e), fm(l), fm(h))
minus <- function(s) gsub("-", MINUS, s, fixed = TRUE)

save_fig <- function(name, w, h, draw) {
  cairo <- isTRUE(capabilities("cairo"))
  if (cairo) grDevices::cairo_pdf(file.path(DIR_FIG, paste0(name, ".pdf")), width = w, height = h, pointsize = 8, family = "sans")
  else grDevices::pdf(file.path(DIR_FIG, paste0(name, ".pdf")), width = w, height = h, pointsize = 8, useDingbats = FALSE)
  draw(); invisible(dev.off())
  args <- list(filename = file.path(DIR_FIG, paste0(name, ".png")), width = w, height = h, units = "in", res = 300,
               pointsize = 8, bg = "white")
  if (cairo) args$type <- "cairo"
  do.call(grDevices::png, args); draw(); invisible(dev.off())
}

# panel at an exact position (inches from the bottom-left corner of the figure)
panel <- function(W, H, x0, x1, y0, y1, xlim, ylim) {
  par(fig = c(x0 / W, x1 / W, y0 / H, y1 / H), mai = c(0, 0, 0, 0), new = TRUE)
  plot.new(); plot.window(xlim = xlim, ylim = ylim, xaxs = "i", yaxs = "i")
  par(xpd = NA)                              # labels may extend outside the panel
}
# x axis with grid, zero line and bottom spine (style shared by Figs 2-5)
xaxis <- function(at, lab, zero = TRUE, digits = 1) {
  u <- par("usr")
  segments(at, u[3], at, u[4], col = GRID, lwd = LW(0.5))
  if (zero) segments(0, u[3], 0, u[4], col = INK2, lwd = LW(0.7))
  segments(u[1], u[3], u[2], u[3], col = INK2, lwd = LW(0.6))
  yin <- grconvertY(u[3], "user", "inches")
  y_tick <- grconvertY(yin - 0.04, "inches", "user"); y_num <- grconvertY(yin - 0.06, "inches", "user")
  y_lab <- grconvertY(yin - 0.24, "inches", "user")
  segments(at, u[3], at, y_tick, col = INK2, lwd = LW(0.6))
  text(at, y_num, minus(formatC(at, format = "f", digits = digits)), adj = c(0.5, 1), col = INK2)
  text(mean(u[1:2]), y_lab, lab, adj = c(0.5, 1), col = INK)
}
ylabels <- function(y, labs, cex = 1) text(par("usr")[1] - 0.02 * diff(par("usr")[1:2]), y, labs, adj = c(1, 0.5), xpd = NA, cex = cex, col = INK)
title_left <- function(s) text(par("usr")[1], par("usr")[4], s, adj = c(0, -0.6), font = 2, cex = CX(8.5), xpd = NA, col = INK)
ci_line <- function(lo, hi, y, col, lw = 1.2) segments(lo, y, hi, y, col = col, lwd = LW(lw))
pts <- function(x, y, col, pch = 21, cex = 1.05) points(x, y, pch = pch, bg = col, col = "white", lwd = LW(0.6), cex = cex)
diamond <- function(lo, m, hi, y, col, hh = 0.32) polygon(c(lo, m, hi, m), c(y, y + hh, y, y - hh), col = col, border = NA)

# ---------------------------------------------------------------- Figure 1: path model (Study 1, person level)
D  <- rt("S1_decomposition_person_level")
DA <- D[D$spec == "attributions", ]; rownames(DA) <- DA$term
DG <- D[D$spec == "attributions + both gaps", ]; rownames(DG) <- DG$term
TE <- rt("S1_traits_expectations"); rownames(TE) <- TE$variable
cia <- function(t, src = DA) fmci(src[t, "est"], src[t, "lo"], src[t, "hi"])

fig1 <- function() {
  par(mar = c(0, 0, 0, 0), family = "sans", col = INK)
  plot.new(); plot.window(xlim = c(0, 100), ylim = c(0, 76), xaxs = "i", yaxs = "i")
  rbox <- function(x, y, w, h, dashed = FALSE, r = 1.2) {
    th <- seq(0, pi / 2, length.out = 8)
    cx <- c(x + w / 2 - r, x - w / 2 + r, x - w / 2 + r, x + w / 2 - r); cy <- c(y + h / 2 - r, y + h / 2 - r, y - h / 2 + r, y - h / 2 + r)
    px <- c(cx[1] + r * cos(th), cx[2] + r * cos(th + pi / 2), cx[3] + r * cos(th + pi), cx[4] + r * cos(th + 3 * pi / 2))
    py <- c(cy[1] + r * sin(th), cy[2] + r * sin(th + pi / 2), cy[3] + r * sin(th + pi), cy[4] + r * sin(th + 3 * pi / 2))
    polygon(px, py, col = "white", border = if (dashed) INK2 else INK, lwd = LW(0.8), lty = if (dashed) "33" else "solid")
  }
  box2 <- function(x, y, w, h, l1, l2 = NULL, dashed = FALSE, bold = FALSE) {
    rbox(x, y, w, h, dashed)
    if (is.null(l2)) text(x, y, l1, cex = 1, font = if (bold) 2 else 1, col = INK)
    else { text(x, y + 1.6, l1, cex = CX(8.5), col = INK); text(x, y - 2.0, l2, cex = CX(7), col = INK2) }
  }
  head <- function(x0, y0, x1, y1, col) {                 # filled arrow head at (x1, y1)
    ix <- grconvertX(c(x0, x1), "user", "inches"); iy <- grconvertY(c(y0, y1), "user", "inches")
    ang <- atan2(diff(iy), diff(ix)); L <- 0.075; Wd <- 0.03
    bx <- ix[2] - L * cos(ang); by <- iy[2] - L * sin(ang)
    hx <- c(ix[2], bx + Wd * sin(ang), bx - Wd * sin(ang)); hy <- c(iy[2], by - Wd * cos(ang), by + Wd * cos(ang))
    polygon(grconvertX(hx, "inches", "user"), grconvertY(hy, "inches", "user"), col = col, border = NA)
    c(grconvertX(bx, "inches", "user"), grconvertY(by, "inches", "user"))
  }
  path <- function(P, dashed = FALSE, col = INK, lw = 0.9) {
    n <- nrow(P); end <- head(P[n - 1, 1], P[n - 1, 2], P[n, 1], P[n, 2], col)
    Q <- P; Q[n, ] <- end
    lines(Q[, 1], Q[, 2], col = col, lwd = LW(lw), lty = if (dashed) "33" else "solid", lend = 1)
  }
  wlab <- function(x, y, s, cex = CX(7), adj = c(0.5, 0.5), col = INK) {
    w <- strwidth(s, cex = cex); h <- strheight(s, cex = cex); px <- 0.4; py <- 0.5
    rect(x - adj[1] * w - px, y - adj[2] * h - py, x + (1 - adj[1]) * w + px, y + (1 - adj[2]) * h + py, col = "white", border = NA)
    text(x, y, s, cex = cex, adj = adj, col = col)
  }
  X0 <- 8.5; XM <- 50; XY <- 93; YC <- 40
  YM <- c(trait = 59, momentary = 44, situational = 29)
  lab <- c(trait = "Trait attribution", momentary = "Momentary internal attribution", situational = "Situational attribution")
  for (m in names(YM)) {
    y <- YM[[m]]
    p0 <- c(X0 + 8.25, YC + (y - YC) * 0.12); p1 <- c(XM - 14.5, y)
    path(rbind(p0, p1)); wlab(mean(c(p0[1], p1[1])), mean(c(p0[2], p1[2])), paste0("a = ", cia(paste0("a_mean_", m))))
    q0 <- c(XM + 14.5, y); q1 <- c(XY - 5.5, YC + (y - YC) * 0.12)
    path(rbind(q0, q1))
    wlab(mean(c(q0[1], q1[1])), mean(c(q0[2], q1[2])),
         sprintf("b = %s (HV), %s (AV)", fm(DA[paste0("b_mean_", m, "_X0"), "est"]), fm(DA[paste0("b_mean_", m, "_X1"), "est"])))
    box2(XM, y, 29, 8, lab[[m]], paste0("indirect ", cia(paste0("ind_mean_", m))))
  }
  path(rbind(c(X0, YC + 4), c(X0, 71), c(XY, 71), c(XY, YC + 3.5)))
  wlab(50, 71, paste0("Direct effect c\u2032 = ", cia("direct")), cex = CX(7.6))
  path(rbind(c(X0, YC - 4), c(X0, 9), c(XM - 19, 9)), dashed = TRUE, col = INK2, lw = 0.8)
  path(rbind(c(XM + 19, 9), c(XY, 9), c(XY, YC - 3.5)), dashed = TRUE, col = INK2, lw = 0.8)
  box2(XM, 9, 38, 8, "Expectation gap", "expected minus current safety and ability", dashed = TRUE)
  box2(X0, YC, 16.5, 8, "Vehicle type\n(AV = 1, HV = 0)", bold = TRUE)
  box2(XY, YC, 11, 7, "Anger", bold = TRUE)
  text(X0 + 1.5, 12.2, sprintf("AV > HV: d = %s (safety),\n%s (ability)", fm(TE["Safety gap (expected - current)", "d"]),
                               fm(TE["Ability gap (expected - current)", "d"])), adj = c(0, 0), cex = CX(6.8), col = INK2)
  text(XY - 1.5, 12.2, sprintf("indirect %s (safety), %s (ability)\nc\u2032 falls to %s", fm(DG["ind_safety_gap", "est"]),
                               fm(DG["ind_ability_gap", "est"]), cia("direct", DG)), adj = c(1, 0), cex = CX(6.8), col = INK2)
  text(50, 19.5, sprintf("Total effect c = %s; total indirect effect via attributions = %s", cia("total"), cia("ind_total")),
       cex = CX(7.3), col = INK)
}
save_fig("Fig1_path_model_study1", 7.4, 4.5, fig1)

# ---------------------------------------------------------------- Figure 2: trait attribution and anger by error (Study 1)
E <- rt("S1_by_event_attributions_anger_blame"); H <- rt("S1_anger_heterogeneity")[1, ]
AN <- E[E$variable == "anger", ]; rownames(AN) <- AN$event
TR <- E[E$variable == "trait", ]; rownames(TR) <- TR$event
ord <- rev(AN$event[order(AN$d)])                 # first = largest d (bottom row)
n1 <- 599; n0 <- 603
se_d <- function(d) sqrt((n1 + n0) / (n1 * n0) + d^2 / (2 * (n1 + n0)))   # approximate SE of d
fig2 <- function() {
  W <- 7.4; Hh <- 3.6; par(family = "sans", col = INK); plot.new()
  yy <- seq_along(ord) - 1; yp <- -1.3; ylim <- c(-2.95, length(ord) - 0.4)
  xl <- 2.1; pw <- 2.5; gap <- 0.14; yb <- 0.5; yt <- Hh - 0.3
  for (k in 1:2) {
    tab <- if (k == 1) TR else AN; col <- if (k == 1) C_TR else INK
    x0 <- xl + (k - 1) * (pw + gap)
    xlim <- if (k == 1) c(-1.5, 0.6) else c(-0.75, 0.6)
    panel(W, Hh, x0, x0 + pw, yb, yt, xlim, ylim)
    xaxis(pretty(xlim)[pretty(xlim) >= xlim[1] & pretty(xlim) <= xlim[2]], "AV \u2212 HV difference (Cohen\u2019s d)")
    d <- tab[ord, "d"]; s <- se_d(d)
    ci_line(d - 1.96 * s, d + 1.96 * s, yy, col, 1.1); pts(d, yy, col)
    title_left(if (k == 1) "A  Trait attribution" else "B  Anger")
    if (k == 1) {
      ylabels(yy, ord)
      m <- mean(TR$d); sm <- sqrt(mean(se_d(TR$d)^2) / nrow(TR))
      diamond(m - 1.96 * sm, m, m + 1.96 * sm, yp, C_TR)
      text(m, yp - 0.62, paste0("mean d = ", fm(m)), adj = c(0.5, 1), cex = CX(6.6), col = INK2)
      text(-1.48, yp, "Average across errors", adj = c(0, 0.5), cex = CX(7.4), col = INK)
    } else {
      segments(H$PI_lo, yp, H$PI_hi, yp, col = INK2, lwd = LW(0.9), lty = "22")
      diamond(H$mean_d_lo, H$mean_d, H$mean_d_hi, yp, INK)
      s1 <- sprintf("mean d = %s [%s, %s]", fm(H$mean_d), fm(H$mean_d_lo), fm(H$mean_d_hi))
      s2 <- sprintf("95%% prediction interval (dashed) %s to %s", fm(H$PI_lo), fm(H$PI_hi))
      txt <- paste(s1, s2, sep = "\n"); w <- strwidth(txt, cex = CX(6.6)); h <- strheight(txt, cex = CX(6.6))
      rect(-0.08 - w / 2 - 0.01, yp - 0.55 - h - 0.05, -0.08 + w / 2 + 0.01, yp - 0.55 + 0.02, col = "white", border = NA)
      text(-0.08, yp - 0.55, txt, adj = c(0.5, 1), cex = CX(6.6), col = INK2)
    }
  }
}
save_fig("Fig2_by_event_trait_anger_study1", 7.4, 3.6, fig2)

# ---------------------------------------------------------------- Figure 3: slopes by vehicle type (Study 1)
S <- rt("S1_multilevel_slopes"); S <- S[S$model == "A" & S$outcome == "anger", ]
fig3 <- function() {
  W <- 7.4; Hh <- 2.25; par(family = "sans", col = INK); plot.new()
  att <- c("trait", "momentary", "situational"); nm <- c("Trait", "Momentary internal", "Situational")
  xl <- 1.15; pw <- 3.0; gap <- 0.14; yb <- 0.48; yt <- Hh - 0.55
  for (k in 1:2) {
    lv <- c("within", "between")[k]; q <- S[S$level == lv, ]; q <- q[match(att, q$attribution), ]
    x0 <- xl + (k - 1) * (pw + gap); xlim <- c(-0.3, 0.42)
    panel(W, Hh, x0, x0 + pw, yb, yt, xlim, c(-0.5, 2.5))
    xaxis(seq(-0.3, 0.4, 0.1), "Slope of anger on attribution (b, 95% CI)")
    yy <- 2:0
    ci_line(q$b_HV_lo, q$b_HV_hi, yy + 0.12, HVC); pts(q$b_HV, yy + 0.12, HVC, 21)
    ci_line(q$b_AV_lo, q$b_AV_hi, yy - 0.12, AVC); pts(q$b_AV, yy - 0.12, AVC, 22)
    for (i in seq_len(nrow(q))) if (q$p_diff_robust[i] < .05) {
      pr <- fmt_p(q$p_diff_robust[i]); pr <- if (grepl("<", pr)) paste("p", pr) else paste("p =", pr)
      text(max(q$b_HV_hi[i], q$b_AV_hi[i], 0) + 0.03, yy[i], sprintf("AV \u2212 HV = %s%s, %s",
           if (q$diff[i] > 0) "+" else "", fmt(q$diff[i], 3), pr), adj = c(0, 0.5), cex = CX(6.8), col = INK2)
    }
    title_left(c("A  Within persons (across errors)", "B  Between persons")[k])
    if (k == 1) ylabels(yy, nm)
  }
  par(fig = c(0, 1, 0, 1), mai = c(0, 0, 0, 0), new = TRUE); plot.new(); plot.window(c(0, 1), c(0, 1))
  legend((xl + pw + gap / 2) / W, 1, legend = c("Human-driven vehicle", "Automated vehicle"),
         pch = c(21, 22), pt.bg = c(HVC, AVC), col = "white", pt.lwd = LW(0.6), pt.cex = 1.05, horiz = TRUE, bty = "n",
         xjust = 0.5, yjust = 1, cex = CX(7.6), text.col = INK, x.intersp = 0.6, seg.len = 1)
}
save_fig("Fig3_slopes_by_vehicle_study1", 7.4, 2.25, fig3)

# ---------------------------------------------------------------- Figure 4: decomposition by error (Study 1)
BD <- rt("S1_decomposition_by_event")
comp <- c("ind_trait", "ind_momentary", "ind_situational", "direct", "total")
PV <- sapply(comp, function(t) vapply(AN$event, function(e) { v <- BD$est[BD$event == e & BD$term == t]; if (length(v)) v else 0 }, numeric(1)))
rownames(PV) <- AN$event
PV <- PV[order(PV[, "total"]), , drop = FALSE]     # smallest total at the bottom
fig4 <- function() {
  W <- 7.4; Hh <- 3.6; par(family = "sans", col = INK); plot.new()
  n <- nrow(PV); yy <- seq_len(n) - 1; h <- 0.62
  panel(W, Hh, 2.05, W - 0.15, 0.5, Hh - 0.45, c(-0.62, 0.62), c(-0.6, n - 0.4))
  xaxis(seq(-0.6, 0.6, 0.2), "Effect of AV (vs HV) on anger, raw scale (1\u20137)")
  cols <- c(C_TR, C_MO, C_SI, C_DI); pos <- rep(0, n); neg <- rep(0, n)
  for (j in 1:4) {
    v <- PV[, j]; left <- ifelse(v >= 0, pos, neg)
    rect(left, yy - h / 2, left + v, yy + h / 2, col = cols[j], border = "white", lwd = LW(1))
    pos <- pos + pmax(v, 0); neg <- neg + pmin(v, 0)
  }
  points(PV[, "total"], yy, pch = 23, bg = INK, col = "white", lwd = LW(0.7), cex = 1.1)
  ylabels(yy, rownames(PV))
  par(fig = c(0, 1, 0, 1), mai = c(0, 0, 0, 0), new = TRUE); plot.new(); plot.window(c(0, 1), c(0, 1))
  legend((2.05 + (W - 0.15)) / 2 / W, 1, legend = c("Indirect via trait", "Indirect via momentary", "Indirect via situational",
         "Direct effect", "Total effect (AV \u2212 HV)"), pch = c(22, 22, 22, 22, 23), pt.bg = c(cols, INK),
         col = c(rep(NA, 4), "white"), pt.cex = c(1.6, 1.6, 1.6, 1.6, 1.1), horiz = TRUE, bty = "n", xjust = 0.5, yjust = 1,
         cex = CX(7), text.col = INK, x.intersp = 0.5)
}
save_fig("Fig4_decomposition_by_event_study1", 7.4, 3.6, fig4)

# ---------------------------------------------------------------- Figure 5: information effects (Study 2)
G <- rt("S2_group_effects"); SE <- rt("S2_by_event")
fig5 <- function() {
  W <- 7.4; Hh <- 2.65; par(family = "sans", col = INK); plot.new()
  outs <- c(trait = "Trait attribution", momentary = "Momentary internal", situational = "Situational attribution",
            anger = "Anger", punishment = "Punishment")
  yb <- 0.48; yt <- Hh - 0.55
  # panel A
  panel(W, Hh, 1.3, 3.1, yb, yt, c(-0.6, 0.3), c(-0.5, 4.5))
  xaxis(seq(-0.6, 0.2, 0.2), "Difference from control (Cohen\u2019s d, 95% CI)")
  yy <- 4:0
  for (k in 1:2) {
    cn <- c("ability vs control", "rule compliance vs control")[k]; q <- G[G$contrast == cn, ]; q <- q[match(names(outs), q$outcome), ]
    off <- c(0.12, -0.12)[k]; col <- c(ABI, RUL)[k]
    ci_line(q$d_lo, q$d_hi, yy + off, col); pts(q$d, yy + off, col, c(21, 22)[k])
  }
  ylabels(yy, outs); title_left("A  Averaged across errors")
  # panel B
  q <- SE[SE$outcome == "anger", ]; evs <- unique(q$event[order(as.numeric(q$event_no))]); yy <- rev(seq_along(evs) - 1)
  panel(W, Hh, 5.3, W - 0.12, yb, yt, c(-1.15, 0.5), c(-0.5, length(evs) - 0.5))
  xaxis(seq(-1.0, 0.5, 0.5), "Difference in anger from control (raw, 95% CI)")
  for (k in 1:2) {
    cn <- c("ability vs control", "rule compliance vs control")[k]; r <- q[q$contrast == cn, ]; r <- r[match(evs, r$event), ]
    off <- c(0.12, -0.12)[k]; col <- c(ABI, RUL)[k]
    ci_line(r$lo, r$hi, yy + off, col); pts(r$diff, yy + off, col, c(21, 22)[k])
  }
  ylabels(yy, evs); title_left("B  Anger by error")
  par(fig = c(0, 1, 0, 1), mai = c(0, 0, 0, 0), new = TRUE); plot.new(); plot.window(c(0, 1), c(0, 1))
  legend(0.5, 1, legend = c("Ability information", "Rule-compliance information"), pch = c(21, 22), pt.bg = c(ABI, RUL),
         col = "white", pt.lwd = LW(0.6), pt.cex = 1.05, horiz = TRUE, bty = "n", xjust = 0.5, yjust = 1, cex = CX(7.6),
         text.col = INK, x.intersp = 0.6)
}
save_fig("Fig5_information_effects_study2", 7.4, 2.65, fig5)
cat("figures written to", normalizePath(DIR_FIG, winslash = "/"), "\n")
