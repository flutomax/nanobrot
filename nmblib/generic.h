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

#ifndef GENERIC_H_
#define GENERIC_H_

#include "floatexp.h"

typedef struct {
	int iters;
	float	trans;
} level;

typedef bool(*callback)(void*, int64_t);


class INmb 
{
public:
	virtual void destroy() = 0;
	virtual void set_re(char* _re) = 0;
	virtual void set_im(char* _im) = 0;
	virtual void set_zoom(char* _zoom) = 0;
	virtual void set_bm(int _bm) = 0;
	virtual void set_bn(int _bn) = 0;
	virtual void set_maxiters(int _maxiters) = 0;
	virtual void set_period(int _period) = 0;
	virtual void get_level(level& _level) = 0;
	virtual void calc(void* owner, callback cbf) = 0;
	virtual bool iteratePtDE2(const double cX, const double cY) = 0;
	virtual void find_period_setup() = 0;
	virtual void find_period_cycle(floatexp& cre, floatexp& cim) = 0;
	virtual void get_radius(floatexp& _radius) = 0;
	virtual int get_zoom_exponent() = 0;
};

typedef struct INmb* HNmb;

#endif // GENERIC_H_
