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

#ifndef KF_FLOATEXP_H
#define KF_FLOATEXP_H

#include <cmath>
#include <math.h>
#include <stdint.h>
#include <limits.h>

#define MAX_PREC 1020
// this has two fewer 0 than you might expect, this is to give headroom for
// avoiding overflow in + and other functions. it is the exponent for 0.0
#define EXP_MIN (-0x80000000000000LL)

class floatexp
{
public:
	double val;
	int64_t exp;

	inline void align()
	{
		if (val != 0)
		{
			union { double d; int64_t i; } u;
			u.d = val;
			exp += ((u.i & 0x7FF0000000000000LL) >> 52) - 1023;
			u.i = (u.i & 0x800FFFFFFFFFFFFFLL) | 0x3FF0000000000000LL;
			val = u.d;
		}
		else
		{
			val = 0;
			exp = EXP_MIN;
		}
	}

	inline floatexp &abs()
	{
		if(val<0)
			val=-val;
		return *this;
	}

	inline void initFromDouble(double a)
	{
		val=a;
		exp=0;
		align();
	}

	inline void initFromLongDouble(long double a)
	{
		using std::frexp;
		int e = 0;
		a = frexp(a, &e);
		val=a;
		exp=e;
		align();
	}

	inline double setExp(double newval,int64_t newexp) const
	{
//		int64_t tmpval = (*((int64_t*)&newval) & 0x800FFFFFFFFFFFFF) | ((newexp+1023)<<52);
//		memcpy(&newval,&tmpval,sizeof(double));
//		return newval;
		union { double d; int64_t i; } u;
		u.d = newval;
		u.i = (u.i & 0x800FFFFFFFFFFFFFLL) | ((newexp + 1023) << 52);
		newval = u.d;
		return newval;
	}

	inline floatexp()
	{
		val = 0;
		exp = EXP_MIN;
	}

	inline floatexp(int a)
	{
		initFromDouble(a);
	}

	inline floatexp(double a)
	{
		initFromDouble(a);
	}

	inline floatexp(double a, int64_t e)
	{
		val = a;
		exp = e;
		align();
	}

	inline floatexp(double a, int64_t e, int dummy)
	{
		(void) dummy;
		val = a;
		exp = e;
	}

	inline floatexp(long double a)
	{
		initFromLongDouble(a);
	}

	inline floatexp &operator =(const floatexp &a)
	{
		val=a.val;
		exp=a.exp;
		return *this;
	}

	inline floatexp &operator =(int a)
	{	
		initFromDouble((double)a);
		return *this;
	}

	inline floatexp &operator =(double a)
	{
		initFromDouble(a);
		return *this;
	}

	inline floatexp &operator =(long double a)
	{
		initFromLongDouble(a);
		return *this;
	}

	inline floatexp operator *(const floatexp &a) const
	{
		floatexp r;
		r.val = a.val*val;
		r.exp = a.exp+exp;
		r.align();
		return r;
	}

	inline floatexp operator /(const floatexp &a) const
	{
		floatexp r;
		r.val = val/a.val;
		r.exp = exp - a.exp;
		r.align();
		return r;
	}

	inline floatexp &mul2()
	{
		exp++;
		return *this;
	}

	inline floatexp &mul4()
	{
		exp+=2;
		return *this;
	}

	inline floatexp operator +(const floatexp &a) const
	{
		floatexp r;
		int64_t diff;
		if(exp>a.exp){
			diff = exp-a.exp;
			r.exp = exp;
			if(diff>MAX_PREC)
				r.val=val;
			else{
				double aval = setExp(a.val,-diff);
				r.val = val+aval;
			}
		}
		else{
			diff = a.exp-exp;
			r.exp = a.exp;
			if(diff>MAX_PREC)
				r.val=a.val;
			else{
				double aval = setExp(val,-diff);
				r.val = a.val+aval;
			}
		}
		r.align();
		return r;
	}

	inline floatexp operator -() const
	{
		floatexp r=*this;
		r.val=-r.val;
		return r;
	}

	inline floatexp &operator +=(const floatexp &a)
	{
		*this = *this+a;
		return *this;
	}

	inline floatexp operator -(const floatexp &a) const
	{
		floatexp r;
		int64_t diff;
		if(exp>a.exp){
			diff = exp-a.exp;
			r.exp = exp;
			if(diff>MAX_PREC)
				r.val = val;
			else{
				double aval = setExp(a.val,-diff);
				r.val = val-aval;
			}
		}
		else{
			diff = a.exp-exp;
			r.exp = a.exp;
			if(diff>MAX_PREC)
				r.val=-a.val;
			else{
				double aval = setExp(val,-diff);
				r.val = aval-a.val;
			}
		}
		r.align();
		return r;
	}

	inline floatexp &operator -=(const floatexp &a)
	{
		*this = *this-a;
		return *this;
	}

	inline bool operator >(const floatexp &a) const
	{
		if(val>0){
			if(a.val<0)
				return true;
			if(exp>a.exp)
				return true;
			else if(exp<a.exp)
				return false;
			return val>a.val;
		}
		else{
			if(a.val>0)
				return false;
			if(exp>a.exp)
				return false;
			else if(exp<a.exp)
				return true;
			return val>a.val;
		}
	}

	inline bool operator <(const floatexp &a) const
	{
		if(val>0){
			if(a.val<0)
				return false;
			if(exp>a.exp)
				return false;
			else if(exp<a.exp)
				return true;
			return val<a.val;
		}
		else{
			if(a.val>0)
				return true;
			if(exp>a.exp)
				return true;
			else if(exp<a.exp)
				return false;
			return val<a.val;
		}
	}

	inline bool operator <=(const int a) const
	{
		floatexp aa(a);
		return (*this<a || *this==a);
	}

	inline bool operator ==(const floatexp &a) const
	{
		if(exp!=a.exp)
			return false;
		return val==a.val;
	}

	inline bool iszero() const
	{
		return (val==0 && exp==0);
	}

	inline double todouble() const
	{
		if(exp<-MAX_PREC || exp>MAX_PREC)
			return 0;
		return setExp(val,exp);
	}

	inline explicit operator double () const
	{
		return todouble();
	}

	inline explicit operator long double() const
	{
		return toLongDouble();
	}

	inline double todouble(int nScaling) const
	{
		if(!nScaling)
			return todouble();
		floatexp ret = *this;
		while(nScaling>9){
			ret.val*=1e10;
			ret.align();
			nScaling-=10;
		}
		while(nScaling>2){
			ret.val*=1e3;
			ret.align();
			nScaling-=3;
		}
		while(nScaling>0){
			ret.val*=1e1;
			ret.align();
			nScaling--;
		}
		while(nScaling<-9){
			ret.val/=1e10;
			ret.align();
			nScaling+=10;
		}
		while(nScaling<-2){
			ret.val/=1e3;
			ret.align();
			nScaling+=3;
		}
		while(nScaling<0){
			ret.val/=1e1;
			ret.align();
			nScaling++;
		}
		if(ret.exp<-MAX_PREC || ret.exp>MAX_PREC)
			return 0;
		return setExp(ret.val,ret.exp);
	}

	inline long double toLongDouble() const
	{
		if (val == 0.0L)
			return 0.0L;
		if (exp >= INT_MAX)
			return (val / 0.0L); // infinity
		if (exp <= INT_MIN)
			return (val * 0.0L); // zero
		return std::ldexp((long double)val, exp);
	}

	inline long double toLongDouble(int nScaling) const
	{
		if (!nScaling)
			return toLongDouble();
		floatexp ret = *this;
		// FIXME risky to go higher than this? 1e300 might be ok?
		while (nScaling > 99) {
			ret.val *= 1e100;
			ret.align();
			nScaling -= 100;
		}
		while (nScaling > 29) {
			ret.val *= 1e30;
			ret.align();
			nScaling -= 30;
		}
		while (nScaling > 9) {
			ret.val *= 1e10;
			ret.align();
			nScaling -= 10;
		}
		while (nScaling > 2) {
			ret.val *= 1e3;
			ret.align();
			nScaling -= 3;
		}
		while (nScaling) {
			ret.val *= 1e1;
			ret.align();
			nScaling--;
		}
		return ret.toLongDouble();
	}

	inline floatexp &operator /=(double a)
	{
		val/=a;
		align();
		return *this;
	}
	inline floatexp &operator *=(double a)
	{
		val*=a;
		align();
		return *this;
	}
	inline floatexp &operator *=(floatexp a)
	{
		return *this = *this * a;
	}
	inline floatexp &operator *=(long double a)
	{
		return *this *= floatexp(a);
	}

	inline floatexp setLongDouble(long double a)
	{
		int e = 0;
		val = std::frexp(a, &e);
		exp = e;
		align();
		return *this;
	}

};

inline floatexp operator*(double a, floatexp b)
{
	return floatexp(a) * b;
}

inline floatexp operator*(floatexp b, double a)
{
	return floatexp(a) * b;
}

inline floatexp operator+(double a, floatexp b)
{
	return floatexp(a) + b;
}

inline floatexp operator+(floatexp b, double a)
{
	return floatexp(a) + b;
}

inline floatexp operator*(int a, floatexp b)
{
	return double(a) * b;
}

inline floatexp abs(floatexp a)
{
	return a.abs();
}

inline floatexp sqrt(floatexp a)
{
	double v;
	int64_t e;
	bool b = (a.exp & 1);

	if (b)
	{
		v = 2.0 * a.val;
		e = (a.exp - 1) / 2;
	}
	else 
	{
		v = a.val;
		e = a.exp / 2;
	}
	v = std::sqrt(v);
	return floatexp(v, e);
	/*
  return floatexp
    ( std::sqrt((a.exp & 1) ? 2.0 * a.val : a.val)
    , (a.exp & 1) ? (a.exp - 1) / 2 : a.exp / 2
    );
	*/
}


#endif
