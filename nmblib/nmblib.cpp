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

// compile:
// g++ -std=c++11 -Ofast -mfpmath=387 -march=native nmblib.cpp -lmpfr -lgmp -Wall -Wextra -pedantic -fopenmp

#include <windows.h>
#include "nmblib.h"
#include "nmbimpl.h"

HNmb nmb_create()
{
	return new TNmb();
}

void nmb_delete(HNmb _nmb)
{
	_nmb->destroy();
}

void nmb_calc(HNmb _nmb, void* owner, callback cbf)
{
	_nmb->calc(owner, cbf);
}

bool nmb_iterate(HNmb _nmb, const double cX, const double cY)
{
	return _nmb->iteratePtDE2(cX, cY);
}

void nmb_get_level(HNmb _nmb, level& _level)
{
	_nmb->get_level(_level);
}

void nmb_set_re(HNmb _nmb, char* _re)
{
	_nmb->set_re(_re);
}

void nmb_set_im(HNmb _nmb, char* _im)
{
	_nmb->set_im(_im);
}

void nmb_set_zoom(HNmb _nmb, char* _zoom)
{
	_nmb->set_zoom(_zoom);
}

void nmb_set_bm(HNmb _nmb, int _bm)
{
	_nmb->set_bm(_bm);
}

void nmb_set_bn(HNmb _nmb, int _bn)
{
	_nmb->set_bm(_bn);
}

void nmb_set_maxiters(HNmb _nmb, int _maxiters)
{
	_nmb->set_maxiters(_maxiters);
}

void nmb_set_period(HNmb _nmb, int _period)
{
	_nmb->set_period(_period);
}

void nmb_find_period_setup(HNmb _nmb)
{
	_nmb->find_period_setup();
}

void nmb_find_period_cycle(HNmb _nmb, floatexp& cre, floatexp& cim)
{
	_nmb->find_period_cycle(cre, cim);
}	

void nmb_get_radius(HNmb _nmb, floatexp& radius)
{
	_nmb->get_radius(radius);
}

int nmb_get_zoom_exponent(HNmb _nmb)
{
	return _nmb->get_zoom_exponent();
}