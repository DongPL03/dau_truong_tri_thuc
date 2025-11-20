import {inject, Injectable} from '@angular/core';
import {ActivatedRouteSnapshot, CanActivateFn, Router, RouterStateSnapshot} from '@angular/router';
import {TokenService} from '../services/token.service';

@Injectable({
  providedIn: 'root'
})
export class GuestGuard {
  constructor(
    private tokenService: TokenService,
    private router: Router,
  ) {
  }

  canActivate(next: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
    const isTokenExpired = this.tokenService.isTokenExpired();
    const isUserIdValid = this.tokenService.getUserId() > 0;

    // Nếu user ĐÃ ĐĂNG NHẬP (Token còn hạn & UserId ok)
    if (!isTokenExpired && isUserIdValid) {
      // 👇 1. Lấy returnUrl từ query params (nếu AuthGuard đã gửi sang)
      const returnUrl = next.queryParams['returnUrl'];

      // 👇 2. Nếu có returnUrl thì quay lại đó, nếu không mới về home
      if (returnUrl) {
        this.router.navigateByUrl(returnUrl).then(r => {
        });
      } else {
        this.router.navigate(['/home']).then(r => {
        });
      }
      return false; // Chặn không cho vào trang Login
    }

    return true; // Cho phép vào trang Login
  }
}

export const GuestGuardFn: CanActivateFn = (next: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean => {
  return inject(GuestGuard).canActivate(next, state);
}
