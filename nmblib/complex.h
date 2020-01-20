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


//minimal implementation
#ifndef MY_COMPLEX_TYPE
#define MY_COMPLEX_TYPE

#include <cmath>

template<typename T>
class fcomplex{
public:
    fcomplex(){re=T(0.); im=T(0.);}
    fcomplex(T rv){re=rv; im=T(0.);}
    fcomplex(T rv, T iv){re=rv; im=iv;}
    fcomplex(const fcomplex<T>& cv){re=cv.re; im=cv.im;}
	template<typename U> fcomplex(const fcomplex<U>& cv){re=T(cv.real()); im=T(cv.imag());}
    fcomplex<T>& operator=(const fcomplex<T>& cv){re=cv.re; im=cv.im; return *this;}
    fcomplex<T>& operator+=(const fcomplex<T>& cv){re+=cv.re; im+=cv.im; return *this;}
    fcomplex<T>& operator-=(const fcomplex<T>& cv){re-=cv.re; im-=cv.im; return *this;}
		fcomplex<T>& operator*=(const fcomplex<T>& cv){
		T re1 = re * cv.re - im * cv.im; 
		   im = re * cv.im + im * cv.re; 
			 re=re1; 
			 return *this;
		}

    T& real(){return re;}
    const T& real()const {return re;}
    T& imag(){return im;}
    const T& imag()const {return im;}
public:
    T re,im;
};

template<typename T>
inline fcomplex<T> operator+(const fcomplex<T> &c1, const fcomplex<T>& c2){
    return fcomplex<T>(c1.re+c2.re, c1.im+c2.im);
}

template<typename T>
inline fcomplex<T> operator+(T c1, const fcomplex<T>& c2){
    return fcomplex<T>(c1+c2.re, c2.im);
}

template<typename T>
inline fcomplex<T> operator+(const fcomplex<T> &c1, T c2){
    return fcomplex<T>(c1.re+c2, c1.im);
}

template<typename T>
inline fcomplex<T> operator-(const fcomplex<T> &c1, const fcomplex<T>& c2){
    return fcomplex<T>(c1.re-c2.re, c1.im-c2.im);
}

template<typename T>
inline fcomplex<T> operator-(const fcomplex<T> &c){
    return fcomplex<T>(-c.re, -c.im);
}

template<typename T>
inline fcomplex<T> operator*(const fcomplex<T>& c1, const fcomplex<T>& c2){
    return fcomplex<T>(c1.re*c2.re-c1.im*c2.im, c1.re*c2.im+c1.im*c2.re);
}

template<typename T>
inline fcomplex<T> operator*(T c1, const fcomplex<T>& c2){
    return fcomplex<T>(c1*c2.re, c1*c2.im);
}

template<typename T>
inline fcomplex<T> operator*(const fcomplex<T>& c2, T c1){
    return fcomplex<T>(c1*c2.re, c1*c2.im);
}

template<typename T>
inline fcomplex<T> operator/(const fcomplex<T> &c1, const fcomplex<T>& c2){
    return T(1.)/norm(c2) * c1*conjugate(c2);
}

template<typename T>
inline fcomplex<T> operator/(const fcomplex<T> &c1, T c2){
    return T(1.)/c2 * c1;
}

template<typename T>
inline fcomplex<T> operator/(T c1, const fcomplex<T>& c2){
    return T(1.)/norm(c2) * c1 * conjugate(c2);
}


//Functions
template<typename T>
inline fcomplex<T> conjugate(const fcomplex<T> &c){
    return fcomplex<T>(c.re, -c.im);
}

template<typename T>
inline fcomplex<T> sqr(const fcomplex<T> &c){
    return fcomplex<T>(c.re*c.re-c.im*c.im, T(2.)*c.re*c.im);
}

template<typename T>
inline T norm(const fcomplex<T> &c){
    return c.re*c.re+c.im*c.im;
}

template<typename T>
inline T abs(const fcomplex<T> &c){
  using std::sqrt;
    return sqrt(norm(c));
}

template<typename T>
inline T real(const fcomplex<T> &c){
    return c.real();
}

template<typename T>
inline T imag(const fcomplex<T> &c){
    return c.imag();
}

template<typename T>
inline fcomplex<T> pow(const fcomplex<T> &c, int p){
    T r = abs(c);
	T theta = std::atan2(c.real(), c.imag());
	r = std::pow(r,p);
	theta *= p;
	return fcomplex<T>(r*cos(theta),r*sin(theta));
}

#endif // COMPLEX

