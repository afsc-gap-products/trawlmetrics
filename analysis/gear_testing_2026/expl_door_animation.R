library(rgl)

source("00_functions.R")

trawl_data_by_haul <- readRDS(file = here::here("data", "trawl_data_by_haul.rds")) |>
  dplyr::filter(FOOTROPE_ON_BOTTOM == "Yes", DOOR_SIZE_M2 == "4.5")

library(rgl)

# ==============================================================================
# 1. PARAMETRIC 4.5-m² NET SYSTEMS HI-LIFT DOOR (NO DUPLICATE CAPS)
# ==============================================================================

# Helper: Open extruded foil profile (no top/bottom caps to avoid doubling)
make_open_foil <- function(x_pts, y_pts, thickness = 0.035, z_min = -0.90, z_max = 0.90) {
  n <- length(x_pts)
  dx <- c(x_pts[2] - x_pts[1], (x_pts[3:n] - x_pts[1:(n-2)]) / 2, x_pts[n] - x_pts[n-1])
  dy <- c(y_pts[2] - y_pts[1], (y_pts[3:n] - y_pts[1:(n-2)]) / 2, y_pts[n] - y_pts[n-1])
  len <- sqrt(dx^2 + dy^2)
  nx <- -dy / len
  ny <-  dx / len
  
  # 4 vertical corner rails
  v_in_b  <- rbind(x_pts - 0.5 * thickness * nx, y_pts - 0.5 * thickness * ny, rep(z_min, n))
  v_out_b <- rbind(x_pts + 0.5 * thickness * nx, y_pts + 0.5 * thickness * ny, rep(z_min, n))
  v_in_t  <- rbind(x_pts - 0.5 * thickness * nx, y_pts - 0.5 * thickness * ny, rep(z_max, n))
  v_out_t <- rbind(x_pts + 0.5 * thickness * nx, y_pts + 0.5 * thickness * ny, rep(z_max, n))
  
  verts <- rbind(cbind(v_in_b, v_out_b, v_in_t, v_out_t), 1)
  
  quads <- list()
  for (i in 1:(n - 1)) {
    # Outer suction face
    quads[[length(quads) + 1]] <- c(n + i, n + i + 1, 3 * n + i + 1, 3 * n + i)
    # Inner pressure face
    quads[[length(quads) + 1]] <- c(i, 2 * n + i, 2 * n + i + 1, i + 1)
  }
  # Leading and trailing vertical edge caps only (no horizontal caps)
  quads[[length(quads) + 1]] <- c(1, n + 1, 3 * n + 1, 2 * n + 1)
  quads[[length(quads) + 1]] <- c(n, 2 * n, 4 * n, 3 * n)
  
  qmesh3d(vertices = verts, indices = matrix(unlist(quads), nrow = 4), homogeneous = TRUE)
}

# Helper: Single continuous bottom wear runner with upturned ski toe
make_bottom_shoe <- function(x_heel = -1.30, x_flat = 0.70, x_toe = 1.35,
                             y_inner = -0.18, y_outer = 0.28,
                             z_base = -1.04, z_deck = -0.90, z_toe_lift = 0.22) {
  xs <- c(x_heel, x_flat, x_toe)
  zb <- c(z_base, z_base, z_base + z_toe_lift)
  zt <- c(z_deck, z_deck, z_deck + z_toe_lift)
  n  <- length(xs)
  
  v_ib <- rbind(xs, rep(y_inner, n), zb)
  v_ob <- rbind(xs, rep(y_outer, n), zb)
  v_it <- rbind(xs, rep(y_inner, n), zt)
  v_ot <- rbind(xs, rep(y_outer, n), zt)
  
  verts <- rbind(cbind(v_ib, v_ob, v_it, v_ot), 1)
  
  quads <- list()
  for (i in 1:(n - 1)) {
    quads[[length(quads) + 1]] <- c(n + i, n + i + 1, 3 * n + i + 1, 3 * n + i)       # Outer
    quads[[length(quads) + 1]] <- c(i, 2 * n + i, 2 * n + i + 1, i + 1)               # Inner
    quads[[length(quads) + 1]] <- c(2 * n + i, 3 * n + i, 3 * n + i + 1, 2 * n + i + 1) # Deck
    quads[[length(quads) + 1]] <- c(i, i + 1, n + i + 1, n + i)                       # Wear bottom
  }
  quads[[length(quads) + 1]] <- c(1, n + 1, 3 * n + 1, 2 * n + 1)                     # Heel cap
  quads[[length(quads) + 1]] <- c(n, 2 * n, 4 * n, 3 * n)                             # Toe cap
  
  qmesh3d(vertices = verts, indices = matrix(unlist(quads), nrow = 4), homogeneous = TRUE)
}

# Build the complete assembly
build_nets_hi_lift <- function(is_starboard = TRUE) {
  # 1. Forward Slat Foil (X = 1.25 down to 0.40)
  s1 <- seq(0, 1, length.out = 12)
  x_slat <- 1.25 - s1 * 0.85
  y_slat <- 0.08 + 0.16 * 4 * s1 * (1 - s1)
  slat <- make_open_foil(x_slat, y_slat, thickness = 0.04, z_min = -0.90, z_max = 0.90)
  
  # 2. Main Foil (X = 0.55 down to -1.25)
  s2 <- seq(0, 1, length.out = 16)
  x_main <- 0.55 - s2 * 1.80
  y_main <- -0.04 + 0.22 * 4 * s2 * (1 - s2)
  main <- make_open_foil(x_main, y_main, thickness = 0.04, z_min = -0.90, z_max = 0.90)
  
  # 3. Single Top Endplate (Caps foils without duplicate surfaces)
  top_plate <- translate3d(scale3d(cube3d(), 1.28, 0.22, 0.015), 0.0, 0.05, 0.915)
  
  # 4. Single Bottom Shoe Runner
  shoe <- make_bottom_shoe()
  
  # 5. Inboard Towing Bridle Bracket (Pressure side: -Y)
  bracket <- translate3d(scale3d(cube3d(), 0.30, 0.02, 0.22), 0.15, -0.16, 0.0)
  
  if (!is_starboard) {
    slat      <- scale3d(slat, 1, -1, 1)
    main      <- scale3d(main, 1, -1, 1)
    top_plate <- scale3d(top_plate, 1, -1, 1)
    shoe      <- scale3d(shoe, 1, -1, 1)
    bracket   <- scale3d(bracket, 1, -1, 1)
  }
  
  list(slat = slat, main = main, top_plate = top_plate, shoe = shoe, bracket = bracket)
}

# ==============================================================================
# 2. 3D TOWING VECTOR ARROW (USING NATIVE rgl::arrow3d)
# ==============================================================================

draw_tow_vector <- function(origin = c(-0.8, -1.1, -1.0), length = 2.2) {
  p_start <- origin
  p_end   <- c(origin[1] + length, origin[2], origin[3])
  
  # type = "rotation" renders a solid 3D cylinder shaft with a conical head
  arr_id <- arrow3d(p0 = p_start, p1 = p_end, 
                    type = "rotation", 
                    col = "#00E5FF", 
                    barblen = 0.30, 
                    width = 0.35)
  
  lbl_id <- text3d(origin[1] + length * 0.5, origin[2], origin[3] + 0.18, 
                   "VESSEL TOW DIRECTION", color = "#00E5FF", cex = 1.0)
  
  invisible(c(arr_id, lbl_id))
}

# ==============================================================================
# 3. ANIMATION & CORRECTED SIGN CONVENTIONS
# ==============================================================================

door_parts <- build_nets_hi_lift(is_starboard = TRUE)

# Telemetry sample
n_frames <- 120
t <- seq(0, 25, length.out = n_frames)
telemetry <- data.frame(
  time_sec  = t,
  pitch_deg = 3.5 + 2.0 * sin(t / 2.5),  # Positive = Nose UP
  roll_deg  = -4.0 + 3.0 * cos(t / 3.0)  # Negative = Inward heel (Starboard)
)

open3d(windowRect = c(50, 50, 1100, 850))
bg3d("gray15")
view3d(theta = 55, phi = 25, zoom = 0.75)

# Coordinate grid
axes3d(edges = "bbox", col = "gray50", labels = TRUE, tick = TRUE)
title3d(xlab = "Forward (X)", ylab = "Athwartships / Spread (Y)", zlab = "Up (Z)", col = "white")

# Draw fixed tow direction indicator
# draw_tow_vector(origin = c(-0.8, -1.1, -1.0), length = 2.2)

active_ids <- NULL

for (i in seq_len(nrow(telemetry))) {
  if (!is.null(active_ids)) {
    pop3d(id = active_ids)
  }
  
  # Standard net-mensuration pitch & roll
  pitch <- telemetry$pitch_deg[i] * (pi / 180)
  roll  <- telemetry$roll_deg[i]  * (pi / 180)
  
  # Corrected Euler rotations:
  # 1. Negative pitch around Y tilts +X upward into +Z (Nose-Up)
  # 2. Roll around X tilts Y and Z
  Ry <- rotationMatrix(-pitch, 0, 1, 0)
  Rx <- rotationMatrix(roll, 1, 0, 0)
  
  # Optional: Nominal operating Angle of Attack (AoA ~ 30 deg to tow path)
  # If CSV pitch/roll is relative to the door body, this aligns it into the water flow
  aoa_rad <- 0 * (pi / 180) # Set to e.g. 28 * (pi/180) if showing yaw relative to track
  Rz <- rotationMatrix(aoa_rad, 0, 0, 1)
  
  R <- Rz %*% Ry %*% Rx
  
  # Transform components
  c_slat    <- transform3d(door_parts$slat, R)
  c_main    <- transform3d(door_parts$main, R)
  c_plate   <- transform3d(door_parts$top_plate, R)
  c_shoe    <- transform3d(door_parts$shoe, R)
  c_bracket <- transform3d(door_parts$bracket, R)
  
  # Render
  id_slat_s <- shade3d(c_slat, color = "#F4B41A", specular = "gray70")
  id_slat_w <- wire3d(c_slat,  color = "#8A6200")
  id_main_s <- shade3d(c_main, color = "#F4B41A", specular = "gray70")
  id_main_w <- wire3d(c_main,  color = "#8A6200")
  id_plate  <- shade3d(c_plate, color = "#5A6B7C") # Steel top plate
  id_shoe   <- shade3d(c_shoe, color = "#222222")  # Heavy ground shoe runner
  id_brack  <- shade3d(c_bracket, color = "#D9534F") # Bridle bracket
  
  # Status readout
  hud_text <- sprintf("Time: %.1fs | Pitch (Nose %s): %.2f deg | Roll (%s): %.2f deg", 
                      telemetry$time_sec[i],
                      ifelse(telemetry$pitch_deg[i] >= 0, "UP", "DN"), abs(telemetry$pitch_deg[i]),
                      ifelse(telemetry$roll_deg[i] <= 0, "IN", "OUT"), abs(telemetry$roll_deg[i]))
  id_hud <- text3d(0, 0, 1.45, hud_text, color = "white", cex = 1.1)
  
  active_ids <- c(id_slat_s, id_slat_w, id_main_s, id_main_w, id_plate, id_shoe, id_brack, id_hud)
  
  Sys.sleep(2)
}

