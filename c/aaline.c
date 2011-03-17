void AALine(int x0, int y0, int x1, int y1)
{
    int addr = (y0*640+x0)*4;
    int dx = x1-x0;
    int dy = y1-y0;
    /* By switching to (u,v), we combine all eight octants */
    if (abs(dx) > abs(dy))
    {
	/* Note: If this were actual C, these integers would be lost
	 * at the closing brace.  That's not what I mean to do.  Do what
	 * I mean. */
	int du = abs(dx);
	int dv = abs(dy);
	int u = x1;
	int v = y1;
	int uincr = 4;
	int vincr = 640*4;
	if (dx < 0) uincr = -uincr;
	if (dy < 0) vincr = -vincr;
    }
    else
    {
	int du = abs(dy);
	int dv = abs(dx);
	int u = y1;
	int v = x1;
	int uincr = 640*4;
	int vincr = 4;
	if (dy < 0) uincr = -uincr;
	if (dx < 0) vincr = -vincr;
    }
    int uend = u + 2 * du;
    int d = (2 * dv) - du;	    /* Initial value as in Bresenham's */
    int incrS = 2 * dv;	/* Δd for straight increments */
    int incrD = 2 * (dv - du);	/* Δd for diagonal increments */
    int twovdu = 0;	/* Numerator of distance; starts at 0 */
    double invD = 1.0 / (2.0*sqrt(du*du + dv*dv));   /* Precomputed inverse denominator */
    double invD2du = 2.0 * (du*invD);   /* Precomputed constant */
    do
    {
	/* Note: this pseudocode doesn't ensure that the address is
	 * valid, or that it even represents a pixel on the same side of
	 * the screen as the adjacent pixel */
	DrawPixelD(addr, twovdu*invD);
	DrawPixelD(addr + vincr, invD2du - twovdu*invD);
	DrawPixelD(addr - vincr, invD2du + twovdu*invD);

	if (d < 0)
	{
	    /* choose straight (u direction) */
	    twovdu = d + du;
	    d = d + incrS;
	}
	else
	{
	    /* choose diagonal (u+v direction) */
	    twovdu = d - du;
	    d = d + incrD;
	    v = v+1;
	    addr = addr + vincr;
	}
	u = u+1;
	addr = addr+uincr;
    } while (u < uend);
}

