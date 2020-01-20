/*
Nanobrot

Copyright(C) 2013 - 2017 Karl Runmo
Copyright(C) 2017 - 2018 Claude Heiland - Allen
Copyright(C) 2019 Vasily Makarov

This program is free software : you can redistribute itand /or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.If not, see < https://www.gnu.org/licenses/>.
*/

#include "nmbimpl.h"


// Conversion routines

inline double r_lo(const R_hi& z, const double& dummy)
{
	(void)dummy;
	return mpfr_get_ld(z.mpfr_srcptr(), MPFR_RNDN);
}

inline long double r_lo(const R_hi& z, const long double& dummy)
{
	(void)dummy;
	return mpfr_get_ld(z.mpfr_srcptr(), MPFR_RNDN);
}

inline floatexp mpfr_get_fe(const mpfr_t value, mpfr_rnd_t r)
{
	signed long int e = 0;
	double l = mpfr_get_d_2exp(&e, value, r);
	return floatexp(l, e);
}

inline floatexp r_lo(const R_hi& z, const floatexp& dummy)
{
	(void)dummy;
	return mpfr_get_fe(z.mpfr_srcptr(), MPFR_RNDN);
}

template<typename R>
inline R r_lo(const char* s, const R& dummy)
{
	return r_lo(R_hi(s), dummy);
}

template <typename R>
inline COMPLEX<R> c_lo(const C_hi& z, const R& dummy)
{
	return COMPLEX<R>(r_lo(real(z), dummy), r_lo(imag(z), dummy));
}

inline double frac(const double& x)
{
	if (x < 0) return 0;
  return x - floor(x);
}

// biPolyClass

biPolyClass::biPolyClass() :
	m_m(16), m_n(8)
{
	init(m_m, m_n);
}

void biPolyClass::init(N m, N n)
{
	for (N l = 0; l <= m_m; l++)
		for (N c = 0; c <= m_n; c++)
			tab[l][c] = C_lo(0);
	tab[1][0] = C_lo(1);
}

void biPolyClass::mcopy()
{
 for (N l = 0; l <= m_m; l++)
	 for (N c = 0; c <= m_n; c++)
		 ttab[l][c] = tab[l][c];
}

C_lo biPolyClass::csqrc(N k, N l) 
{
	C_lo v(0);
	for (N i = 0; i <= k; i++)
		for (N j = 0; j <= l; j++)
			v += ttab[i][j] * ttab[k - i][l - j];
	return v;
}

void biPolyClass::sqr() 
{
	mcopy();
	for (N i = 0; i <= m_m; i++)
		for (N j = 0; j <= m_n; j++)
			tab[i][j] = csqrc(i, j);
}

void biPolyClass::cstep(C_lo z) 
{
	sqr();
	tab[0][0] = z;
	tab[0][1] += C_lo(1);
}

C_lo biPolyClass::eval(C_lo u, C_lo v) 
{
	C_lo r(0);
	C_lo ui(1);
	for (N i = 0; i <= m_m; i += 2) {
		C_lo vj(ui);
		for (N j = 0; j <= m_n; j++) {
			r += tab[i][j] * vj;
			vj *= v;
		}
		ui *= u * u;
	}
	return r;
}

C_lo biPolyClass::eval_dc(C_lo u, C_lo v) 
{
	C_lo r(0);
	C_lo ui(1);
	for (N i = 0; i <= m_m; i += 2) {
		C_lo vj(ui);
		for (N j = 1; j <= m_n; j++) {
			r += floatexp(j) * tab[i][j] * vj;
			vj *= v;
		}
		ui *= u * u;
	}
	return r;
}

C_lo biPolyClass::eval_dz(C_lo u, C_lo v) 
{
	C_lo r(0);
	C_lo ui(u);
	for (N i = 2; i <= m_m; i += 2) {
		C_lo vj(C_lo(i) * ui);
		for (N j = 0; j <= m_n; j++) {
			r += tab[i][j] * vj;
			vj *= v;
		}
		ui *= u * u;
	}
	return r;
}

floatexp biPolyClass::getRadius() 
{
	//return abs(tab[0][1])/abs(tab[0][2]);
	floatexp r(0);
	for (int i = 0; i < 10; i++) {
		C_lo den(0);
		floatexp rr(1);
		for (int j = 2; j <= m_n; j++) {
			den += tab[0][j] * rr;
			rr *= r;
		}
		r = abs(tab[0][1]) / abs(den);
	}
	return floatexp(0.5)* r;
}

// perturbationClassD

perturbationClassD::perturbationClassD(C_lo d0, C_lo d, C_lo dd, N n0) : 
	m_d0(d0), m_d(d), m_dd(dd), m_n0(n0), m_escaped(false) 
{
}

void perturbationClassD::run(const refClass ref, N maxiter) {
	for (N i = m_n0; i < maxiter; i++) {
		C_lo zn(ref[i]);
		m_dd = floatexp(2) * m_dd * (m_d + zn) + floatexp(1);
		m_d = m_d * (floatexp(2) * zn + m_d) + m_d0;
		floatexp r(norm(zn + m_d));
		if (r > floatexp(1e10)) {
			m_escaped = true;
			double j = i + 1 - log(log(double(r))) / log(2.0);
			m_iters = int(trunc(j));
			m_trans = 1 - frac(j);
			return;
		}
	}
}

// uniPolyClass

uniPolyClass::uniPolyClass(const biPolyClass& p, C_lo c): 
	m_m(p.m_m) 
{
	for (N i = 0; i <= m_m; i += 2)
	{
		C_lo s(0), ds(0), cj(1), cj1(0);
		for (N j = 0; j <= p.m_n; ++j)
		{
			s += p.tab[i][j] * cj;
			ds += C_lo(j) * p.tab[i][j] * cj1;
			cj *= c;
			cj1 *= c;
			if (j == 0) cj1 = C_lo(1);
		}
		b[i] = s;
		dbdc[i] = ds;
	}
}

void uniPolyClass::eval(C_lo& z) const 
{
	C_lo zs(0), zi(1);
	for (N i = 0; i <= m_m; i += 2)
	{
		zs += b[i] * zi;
		zi *= z * z;
	}
	z = zs;
}

void uniPolyClass::eval(C_lo& z, C_lo& dc) const 
{
	C_lo zs(0), dcs(0), zi(1), zi1(0);
	for (N i = 0; i <= m_m; i += 2)
	{
		dcs += C_lo(i) * b[i] * zi1 * dc + dbdc[i] * zi;
		zs += b[i] * zi;
		zi *= z * z;
		zi1 *= z * z;
		if (i == 0) zi1 = z;
	}
	z = zs;
	dc = dcs;
}

void uniPolyClass::eval_dz(C_lo & z, C_lo & dz) const 
{
	C_lo zs(0), dzs(0), zi(1), zi1(0);
	for (N i = 0; i <= m_m; i += 2)
	{
		dzs += C_lo(i) * b[i] * zi1 * dz;
		zs += b[i] * zi;
		zi *= z * z;
		zi1 *= z * z;
		if (i == 0) zi1 = z;
	}
	z = zs;
	dz = dzs;
}

void uniPolyClass::eval(C_lo & z, C_lo & dz, C_lo & dc, C_lo & dzdz, C_lo & dcdz) const 
{
	C_lo zs(0), dzs(0), dcs(0), dzdzs(0), dcdzs(0), zi(1), zi1(0), zi2(0);
	for (N i = 0; i <= m_m; i += 2)
	{
		dcdzs += C_lo(i) * C_lo(i - 1) * b[i] * zi2 * dz * dc + C_lo(i) * b[i] * zi1 * dcdz + C_lo(i) * dbdc[i] * zi1 * dz;
		dzdzs += C_lo(i) * C_lo(i - 1) * b[i] * zi2 * dz * dz + C_lo(i) * b[i] * zi1 * dzdz;
		dcs += C_lo(i) * b[i] * zi1 * dc + dbdc[i] * zi;
		dzs += C_lo(i) * b[i] * zi1 * dz;
		zs += b[i] * zi;
		zi *= z * z;
		zi1 *= z * z;
		zi2 *= z * z;
		if (i == 0) zi1 = z;
		if (i == 0) zi2 = C_lo(1);
	}
	z = zs;
	dz = dzs;
	dc = dcs;
	dzdz = dzdzs;
	dcdz = dcdzs;
}


// tmpPolyClass

tmpPolyClass::tmpPolyClass(const biPolyClass& p) : 
	m_m(p.m_n) 
{
	for (N i = 0; i <= m_m; i++)
		b[i] = p.tab[0][i];
}

//evaluation function. It would be nice to add an ()-operator... or not. :)
C_lo tmpPolyClass::eval(C_lo u) 
{
	C_lo r(0);
	C_lo ui(1);
	for (N i = 0; i <= m_m; i++) {
		r += b[i] * ui;
		ui *= u;
	}
	return r;
}

//evaluate derivative.
C_lo tmpPolyClass::evalD(C_lo u) 
{
	C_lo r(0);
	C_lo ui(1);
	for (N i = 1; i <= m_m; i++) 
	{
		r += floatexp(i) * b[i] * ui;
		ui *= u;
	}
	return r;
}

//Gives the nearest root to the 0. To use if and when applicable (that is the reference is near 0... atom domain thing!)
//Newton should do the job (otherwise IA-Newton ?).
C_lo tmpPolyClass::getRoot() 
{
	C_lo rt(0);
	//R_lo t = abs(eval(rt));
	for (N i = 0; i < 30; i++) 
	{
		C_lo num = eval(rt);
		C_lo den = evalD(rt);
		C_lo delta = num / den;
		num = rt;
		rt -= delta;
		if (rt.re == num.re && rt.im == num.im) break;
	}
	return rt;
}

// nmb (Nano MB class)

TNmb::TNmb() :
	maxiters(1000), 
	bm(16), 
	bn(8), 
	precision(1000), 
	period(1),
	sre(0),
	sim(0),
	szoom(0),
	fzoom(0),
	tmax(2),
	radius(2),
	diametr(4),
	Bout(2),
	fp()
{

}

void TNmb::precalc()
{
	fzoom = szoom;
	radius = floatexp(2) / r_lo(szoom, floatexp(0));
	diametr = radius * 2;
	tmax = radius;
	precision = N(-tmax.exp) + 64;
	mpreal::set_default_prec(precision);
	zPos = C_hi(0, 0);
	centerPos = C_hi(R_hi(sre), R_hi(sim));
}

void TNmb::calc(void* owner, callback cbf)
{
	precalc();
	fp.init(bm, bn);
	ref.clear();
	ref1.clear();
	C_lo zlo(0);
	int64_t lastpc = -1;
	for (N i = 0; i < period; i++) {
		ref.adde(zlo);
		zPos = zPos * zPos + centerPos;
		zlo = c_lo(zPos, tmax);
		fp.cstep(zlo);
		int64_t pc = ((i + 1) * 1000) / period;
		if (pc > lastpc)
		{
			if (cbf(owner, pc))
			{	
				break;
			}
			lastpc = pc;
		}
	}

	Bout = fp.getRadius();

	//--------------------------------------------------------------------------------------
	//In case the location is not "exactly" at a nucleus, we need to "correct" the perturbation reference.
	//This is because the reference orbit is computed for only one period. If the reference c is outside the mbset, it will eventually escape or be likely off 0.
	//Find the nucleus of the minibrot.
	tmpPolyClass tp(fp);
	nucleusPos = tp.getRoot();
	//cout << "----------------" << endl << "nucleus rel pos: " << nucleusPos << endl;
	//Rebase the reference orbit to the nucleus
	
	C_lo zlo1(0);
	for (N i = 0; i < period; i++) {
		zlo = ref[i];
		ref1.adde(zlo + zlo1);
		zlo1 = zlo1 * (zlo1 + floatexp(2) * zlo) + nucleusPos;
	}
	//At this point zlo+zlo1 should be very close to 0
	//zlo = c_lo(z, tmax);
}

bool TNmb::iteratePtDE2(const double cX, const double cY)
{
	C_lo d0(cX, cY);
	C_lo d(0);
	C_lo dd(0);
	N i(0);
	N si(0);
	floatexp d_norm(INFINITY);	// 1.0 / 0.0
	d0 = diametr * d0;
	if (abs(d0) < Bout) {
		uniPolyClass up(fp, d0);
		while (i < maxiters && norm(d) < Bout) {
			up.eval(d, dd);
			i += period;
			if (norm(d) < d_norm)
			{
				d_norm = norm(d);
				// check interior: newton iterations for w0 = f^(si*p)(w0, d0)
				C_lo w0(d);
				C_lo dw;
				bool converged = false;//------------------
				for (int step = 0; step < 16; ++step)
				{
					C_lo w(w0);
					dw = C_lo(1);
					for (int n = 0; n < si; ++n)
					{
						up.eval_dz(w, dw);
					}
					C_lo w0_next = w0 - (w - w0) / (dw - C_lo(1));
					//R_lo delta = norm(w0_next - w0);
					C_lo delta = (w0_next - w0);//-----------------
					w0 = w0_next;
					//R_lo epsilon2(0); // FIXME
					//if (delta <= epsilon2) break; // converged
					if (abs(delta.re) < abs(w0.re) * floatexp(1e-6) && abs(delta.im) < abs(w0.im) * floatexp(1e-6))
					{
						converged = true;
						break;
					}//-----------------------
				}
				if (converged && norm(dw) < floatexp(1))
				{
					// is interior, calculate interior DE
					flevel.iters = maxiters;
					flevel.trans = 0.0f;
					return false;
				}
			}
		}
	} //else dd = C_lo(1);//knighty: That was a mistake. We begin iterating at 0 so derivative doesn't have to be changed. 
	if (i >= maxiters)
	{

		flevel.iters = maxiters;
		flevel.trans = 0.0f;
		return false;
	}
	//d0 and d have to be transformed into ref frame.
	C_lo d0_(d0 - nucleusPos);
	d -= ref1[i];
	perturbationClassD p(d0_, d, dd, i);
	p.run(ref1, maxiters);
	if (p.haveEscaped())
	{
		flevel.iters = p.getIters();
		flevel.trans = p.getTrans();
		return true;
	}
	else
	{
		flevel.iters = maxiters;
		flevel.trans = 0.0f;
		return false;
	}
	return false;
}

void TNmb::find_period_setup()
{
	precalc();
}
	
void TNmb::find_period_cycle(floatexp& cre, floatexp& cim)
{
	zPos = zPos * zPos + centerPos;
	cre = r_lo(real(zPos), tmax);
	cim = r_lo(imag(zPos), tmax);
}

void TNmb::get_radius(floatexp& _radius)
{
	_radius = radius;
}

int TNmb::get_zoom_exponent()
{
	return int(log10(fzoom, mpreal::get_default_rnd()));
}
