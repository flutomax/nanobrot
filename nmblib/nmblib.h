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

#ifndef NMBLIB_H_
#define NMBLIB_H_

#include "generic.h"

#define EXPORT __declspec(dllexport)

#ifdef __cplusplus
#define EXTERN_C extern "C"
#endif // __cplusplus

	// function to be exported
  EXTERN_C EXPORT HNmb __cdecl nmb_create();
	EXTERN_C EXPORT void __cdecl nmb_delete(HNmb _nmb);
	EXTERN_C EXPORT void __cdecl nmb_calc(HNmb _nmb, void* ownerowner, callback cbf);
	EXTERN_C EXPORT bool __cdecl nmb_iterate(HNmb _nmb, const double cX, const double cY);
	EXTERN_C EXPORT void __cdecl nmb_get_level(HNmb _nmb, level& _level);
	EXTERN_C EXPORT void __cdecl nmb_set_re(HNmb _nmb, char* _re);
	EXTERN_C EXPORT void __cdecl nmb_set_im(HNmb _nmb, char* _im);
	EXTERN_C EXPORT void __cdecl nmb_set_zoom(HNmb _nmb, char* _zoom);
	EXTERN_C EXPORT void __cdecl nmb_set_bm(HNmb _nmb, int _bm);
	EXTERN_C EXPORT void __cdecl nmb_set_bn(HNmb _nmb, int _bn);
	EXTERN_C EXPORT void __cdecl nmb_set_maxiters(HNmb _nmb, int _maxiters);
	EXTERN_C EXPORT void __cdecl nmb_set_period(HNmb _nmb, int _period);
	EXTERN_C EXPORT void __cdecl nmb_find_period_setup(HNmb _nmb);
	EXTERN_C EXPORT void __cdecl nmb_find_period_cycle(HNmb _nmb, floatexp& cre, floatexp& cim);
	EXTERN_C EXPORT void __cdecl nmb_get_radius(HNmb _nmb, floatexp& radius);
	EXTERN_C EXPORT int __cdecl nmb_get_zoom_exponent(HNmb _nmb);

#endif // NMBLIB_H_
