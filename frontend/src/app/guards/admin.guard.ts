import {inject, Injectable} from '@angular/core';
import {ActivatedRouteSnapshot, CanActivateFn, Router, RouterStateSnapshot} from '@angular/router'; // Đảm bảo bạn đã import Router ở đây.
import {TokenService} from '../services/token.service';
import {UserResponse} from '../responses/nguoidung/user-response';
import {UserService} from '../services/user.service';

@Injectable({
  providedIn: 'root'
})
export class AdminGuard {
  userResponse?: UserResponse | null;

  constructor(
    private tokenService: TokenService,
    private router: Router,
    private userService: UserService
  ) {
  }

  canActivate(next: ActivatedRouteSnapshot, state: RouterStateSnapshot): boolean {
    const isTokenExpired = this.tokenService.isTokenExpired();
    const isUserIdValid = this.tokenService.getUserId() > 0;
    this.userResponse = this.userService.getUserResponseFromLocalStorage();
    const roleName = this.userResponse?.vai_tro?.ten_vai_tro?.toUpperCase();
    const isAdmin = roleName === 'ROLE_ADMIN' || roleName === 'ADMIN';
    if (!isTokenExpired && isUserIdValid && isAdmin) {
      return true;
    } else {
      // Nếu không authenticated, bạn có thể redirect hoặc trả về một UrlTree khác.
      // Ví dụ trả về trang dang-nhap:
      this.router.navigate(['/login']);
      return false;
    }
  }
}

export const AdminGuardFn: CanActivateFn = (
  next: ActivatedRouteSnapshot,
  state: RouterStateSnapshot
): boolean => {
  return inject(AdminGuard).canActivate(next, state);
}
