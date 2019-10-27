x = [0 600 1200 1800];
xx = 0:20:1800;

z1 = [30 20 15 30];
zz1 = spline(x,z1,xx);
plot(x,z1,'o',xx,zz1)

z2 = [15 100 140 350];
zz2 = spline(x,z2,xx);
plot(x,z2,'o',xx,zz2)

z3 = [10,95,200,120];
zz3 = spline(x,z3,xx);
plot(x,z3,'o',xx,zz3)

z4 = [20,45,25,35];
zz4 = spline(x,z4,xx);
plot(x,z4,'o',xx,zz4)

y = [ 0 500 1000 1500 ];
yy = 0:20:1500;

z5 = [ 30 15 10 20 ];
zz5 = spline(y,z5,yy);
plot(y,z5,'o',yy,zz5);
