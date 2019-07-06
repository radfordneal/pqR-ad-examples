# Find the distance travelled by a projectile launched on level
# ground with initial velocity (vx,vy), with no air resistance.

distance_travelled <- function (vx, vy, dt=0.0001, g=9.8) {
    x <- y <- 0
    repeat {
        last_x <- x
        last_y <- y
        x <- x + vx*dt
        y <- y + vy*dt
        if (y < 0)  # return impact x location, interpolating
            return ((x*last_y - last_x*y) / (last_y-y))
        vy <- vy - g*dt
    }
}


# See the derivative of distance with respect to angle of launch for 
# angles of 40 and 60 degrees.

with gradient (a = 40 * pi/180) distance_travelled (cos(a), sin(a))
with gradient (a = 60 * pi/180) distance_travelled (cos(a), sin(a))


# Find angle of launch maximizing distance, for fixed initial speed.
# Note that nlm minimizes, so we negate the distance.

system.time(print(
nlm (function (a) 
       with gradient (a) -distance_travelled (cos(a), sin(a)),
     0) $ estimate * 180/pi
))


# For this simple example with only one variable, optimizing without
# the gradient information is about as fast...

system.time(print(
nlm (function (a) 
       -distance_travelled (cos(a), sin(a)),
     0) $ estimate * 180/pi
))
