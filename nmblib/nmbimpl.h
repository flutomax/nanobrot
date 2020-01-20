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

#ifndef NMBIMPL_H_
#define NMBIMPL_H_

#include "generic.h"
#include <windows.h>
#include <vector>
#include "complex.h"
#include "mpreal.h"

#define COMPLEX fcomplex

using namespace mpfr;

typedef mpreal R_hi;
typedef COMPLEX<R_hi> C_hi;

using namespace std;
typedef int N;
typedef COMPLEX<floatexp> C_lo;

class uniPolyClass;
class tmpPolyClass;

class biPolyClass 
{
private:
	friend class uniPolyClass;
	friend class tmpPolyClass;
	N m_m, m_n;
	C_lo tab[128][128];
	C_lo ttab[128][128];
	void mcopy();
	C_lo csqrc(N k, N l);
public:
	biPolyClass();
	~biPolyClass() {}
	void init(N m, N n);
	void sqr(); 
	void cstep(C_lo z);
	C_lo eval(C_lo u, C_lo v);
	C_lo eval_dc(C_lo u, C_lo v);
	C_lo eval_dz(C_lo u, C_lo v);
	floatexp getRadius();
};

class refClass {
	N m_n;
	vector<C_lo> m_v;
public:
	refClass() : m_n(0), m_v(0) {}
	void clear() { m_v.clear();	m_n = 0;}
	void adde(C_lo c) { m_v.push_back(c); m_n++; }
	const C_lo& operator[](N i) const { return m_v[i % m_n]; }
};

class perturbationClassD 
{
private:
	C_lo m_d0;
	C_lo m_d;
	C_lo m_dd;
	N    m_n0;
	bool m_escaped;
	int   m_iters;
	float m_trans;
public:
	perturbationClassD(C_lo d0, C_lo d, C_lo dd, N n0);
	~perturbationClassD() {}
	void run(const refClass ref, N maxiter);
	int   getIters() { return m_iters; }
	float getTrans() { return m_trans; }
	bool haveEscaped() { return m_escaped; }
};

class uniPolyClass 
{
private:
	N m_m;
	C_lo b[128];
	C_lo dbdc[128];
public:
	uniPolyClass(const biPolyClass& p, C_lo c);
	~uniPolyClass() {}
	void eval(C_lo& z) const;
	void eval(C_lo& z, C_lo& dc) const;
	void eval_dz(C_lo& z, C_lo& dz) const;
	void eval(C_lo& z, C_lo& dz, C_lo& dc, C_lo& dzdz, C_lo& dcdz) const;
};

class tmpPolyClass 
{
private:
	N m_m;
	C_lo b[128];
public:
	tmpPolyClass(const biPolyClass& p);
	~tmpPolyClass() {}
	C_lo eval(C_lo u);
	C_lo evalD(C_lo u);
	C_lo getRoot();
};

class TNmb: public INmb
{
private:
	N maxiters;
	N bm;
	N bn;
	N precision;
	N period;
	char* sre;
	char* sim;
	char* szoom;
	R_hi fzoom;
	floatexp radius;
	floatexp tmax;
	floatexp Bout;
	level flevel;
	biPolyClass fp;
	refClass ref;
	refClass ref1;
	C_lo diametr;
	C_lo nucleusPos;
	C_hi centerPos;
	C_hi zPos;
	void precalc();
public:
	TNmb();
	~TNmb() {}
	void destroy() { delete this; }
	void set_re(char* _re) { sre = _re;	}
	void set_im(char* _im) { sim = _im; }
	void set_zoom(char* _zoom) { szoom = _zoom; }
	void set_bm(int _bm) { bm = _bm; }
	void set_bn(int _bn) { bm = _bn; }
	void set_maxiters(int _maxiters) { maxiters = _maxiters; }
	void set_period(int _period) { period = _period; }
	void get_level(level& _level)	{	_level = flevel; }
	void calc(void* owner, callback cbf);
	bool iteratePtDE2(const double cX, const double cY);	
	void find_period_setup();
	void find_period_cycle(floatexp& cre, floatexp& cim);
	void get_radius(floatexp& _radius);
	int get_zoom_exponent();
};


#endif // NMBIMPL_H_

