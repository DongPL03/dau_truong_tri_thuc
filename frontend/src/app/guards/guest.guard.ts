// bên trong file: guest.guard.ts
import {inject, Injectable} from '@angular/core';
import {ActivatedRouteSnapshot, CanActivate, CanActivateFn, Router, RouterStateSnapshot} from '@angular/router';
import {TokenService} from '../services/token.service';

// ... (code class GuestGuard của bạn ở đây) ...
@Injectable({
  providedIn: 'root'
})
export class GuestGuard implements CanActivate {
  constructor(
    private tokenService: TokenService,
    private router: Router,
  ) {
  }

  canActivate(next: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
    if (this.tokenService.isAuthenticated()) {
      this.router.navigate(['/home']);
      return false;
    } else {
      return true;
    }
  }
}

// === DÁN ĐOẠN NÀY VÀO CUỐI FILE ===
export const GuestGuardFn: CanActivateFn = (
  next: ActivatedRouteSnapshot,
  state: RouterStateSnapshot
): boolean => {
  return inject(GuestGuard).canActivate(next, state);
}
