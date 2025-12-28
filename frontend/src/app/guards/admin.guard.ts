import {inject, Injectable} from '@angular/core';
import {ActivatedRouteSnapshot, CanActivateFn, Router, RouterStateSnapshot} from '@angular/router';
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

    // 1. Kiểm tra đăng nhập trước
    if (isTokenExpired || !isUserIdValid) {
      // Nếu chưa đăng nhập -> Về Login kèm link cũ để đăng nhập xong quay lại
      this.router.navigate(['/login'], {queryParams: {returnUrl: state.url}});
      return false;
    }

    // 2. Lấy thông tin User từ LocalStorage (QUAN TRỌNG: Phải chắc chắn Login đã lưu cái này)
    this.userResponse = this.userService.getUserResponseFromLocalStorage();

    // Debug: Kiểm tra xem reload xong có lấy được role không
    // Bấm F12 xem tab Console khi reload
    console.log('🔍 AdminGuard Check:', this.userResponse);

    const roleName = this.userResponse?.vai_tro?.ten_vai_tro?.toUpperCase();
    const isAdmin = roleName === 'ROLE_ADMIN' || roleName === 'ADMIN';

    // 3. Kiểm tra quyền Admin
    if (isAdmin) {
      return true; // ✅ Cho qua
    } else {
      // 🛑 Đã đăng nhập nhưng KHÔNG PHẢI ADMIN
      // Đừng đẩy về Login (vì sẽ bị GuestGuard đá về Home)
      // Hãy đẩy về Home hoặc trang báo lỗi 403
      console.log('Bạn không có quyền truy cập trang quản trị!');
      this.router.navigate(['/home']);
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
