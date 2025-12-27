import {CommonModule} from '@angular/common';
import {Component, OnInit} from '@angular/core';
import Swal from 'sweetalert2';
import {UserResponse} from '../../../../responses/nguoidung/user-response';
import {UserSummaryResponse} from '../../../../responses/nguoidung/user-summary-response';
import {ResponseObject} from '../../../../responses/response-object';
import {LichSuTranDauResponse} from '../../../../responses/trandau/lichsutrandau';
import {Base} from '../../../base/base';

@Component({
  selector: 'app-user-detail',
  imports: [CommonModule],
  standalone: true,
  templateUrl: './user-detail.html',
  styleUrl: './user-detail.scss',
})
export class UserDetail extends Base implements OnInit {
  user: UserResponse | null = null;
  summary: UserSummaryResponse | null = null;
  battleHistory: LichSuTranDauResponse[] = [];

  selected_user_id: number | undefined;

  user_history_loading = false;
  user_history_items: LichSuTranDauResponse[] = [];
  user_history_page = 0;
  user_history_limit = 5;
  user_history_total_pages = 0;

  loading = false;
  currentAdminId: number = 0;

  ngOnInit(): void {
    this.currentAdminId = this.tokenService.getUserId();
    const idParam = this.route.snapshot.paramMap.get('id');
    const userId = idParam ? +idParam : 0;
    this.selected_user_id = userId;
    this.loadUser(userId);
    this.loadSummary(userId);
  }

  loadUser(userId: number): void {
    this.loading = true;
    this.userService.getUserById(userId).subscribe({
      next: (res: ResponseObject<UserResponse>) => {
        console.log('User detail:', res.data);
        this.user = res.data || null;
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải thông tin người dùng', 'error').then((r) => {
        });
      },
    });
  }

  loadSummary(userId: number): void {
    this.loading = true;
    this.userService.getUserSummary(userId).subscribe({
      next: (res: ResponseObject<UserSummaryResponse>) => {
        this.summary = res.data || null;
        this.loadUserHistory(0);
        if (this.summary) {
          const {so_tran_thang, tong_tran} = this.summary;
          this.summary.ti_le_thang = tong_tran > 0 ? so_tran_thang / tong_tran : 0;
        }
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải thông tin tổng quan người dùng', 'error');
      },
    });
  }

  loadUserHistory(page: number = 0) {
    this.tranDauService
      .getUserHistory(this.selected_user_id!, page, this.user_history_limit)
      .subscribe({
        next: (res) => {
          this.user_history_loading = false;
          const p = res.data!;
          this.user_history_items = p.items || [];
          this.battleHistory = this.user_history_items;
          console.log('User battle history:', this.battleHistory);
        },
        error: (err) => {
          this.user_history_loading = false;
        },
      });
  }

  goBack(): void {
    this.router.navigate(['/admin/users']).then((r) => {
    });
  }

  // Actions
  get isCurrentUser(): boolean {
    return this.user?.id === this.currentAdminId;
  }

  resetPassword(): void {
    if (!this.user) return;
    const user = this.user;

    Swal.fire({
      title: 'Reset mật khẩu?',
      text: `User: ${user.ten_dang_nhap || user.email || user.ho_ten}`,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Reset',
      cancelButtonText: 'Huỷ',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.userService.resetUserPassword(user.id).subscribe({
        next: (res: ResponseObject<string>) => {
          const newPass = res.data;
          Swal.fire('Thành công', `Mật khẩu mới: <b>${newPass}</b>`, 'success');
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể reset mật khẩu', 'error');
        },
      });
    });
  }

  blockOrEnable(active: boolean): void {
    if (!this.user) return;

    if (this.isCurrentUser && !active) {
      Swal.fire('Cảnh báo', 'Bạn không thể khóa chính tài khoản của mình', 'warning');
      return;
    }

    const actionText = active ? 'mở khoá' : 'khoá';
    Swal.fire({
      title: `Xác nhận ${actionText} tài khoản?`,
      text: `User: ${this.user.ten_dang_nhap || this.user.email || this.user.ho_ten}`,
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Đồng ý',
      cancelButtonText: 'Huỷ',
    }).then((result) => {
      if (!result.isConfirmed) return;

      if (!this.user) return;
      const userId = this.user.id;

      this.userService.blockOrEnableUser(userId, active).subscribe({
        next: (res: ResponseObject) => {
          Swal.fire(
            'Thành công',
            res.message || 'Cập nhật trạng thái tài khoản thành công',
            'success'
          );
          this.loadUser(userId);
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể cập nhật trạng thái tài khoản', 'error');
        },
      });
    });
  }

  changeRole(role: string): void {
    if (!this.user) return;

    if (this.isCurrentUser) {
      Swal.fire('Cảnh báo', 'Bạn không thể thay đổi vai trò của chính mình', 'warning');
      return;
    }

    const actionText = role === 'ADMIN' ? 'Nâng quyền ADMIN' : 'Chuyển về USER';
    Swal.fire({
      title: `${actionText}?`,
      text: `User: ${this.user.ten_dang_nhap || this.user.email || this.user.ho_ten}`,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Đồng ý',
      cancelButtonText: 'Huỷ',
    }).then((result) => {
      if (!result.isConfirmed) return;

      if (!this.user) return;
      const userId = this.user.id;

      this.userService.updateUserRole(userId, role).subscribe({
        next: (res: ResponseObject) => {
          Swal.fire('Thành công', res.message || 'Cập nhật vai trò thành công', 'success');
          this.loadUser(userId);
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể cập nhật vai trò', 'error');
        },
      });
    });
  }

  restore(): void {
    if (!this.user) return;

    Swal.fire({
      title: 'Khôi phục tài khoản?',
      text: `User: ${this.user.ten_dang_nhap || this.user.email || this.user.ho_ten}`,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Khôi phục',
      cancelButtonText: 'Huỷ',
    }).then((result) => {
      if (!result.isConfirmed) return;

      if (!this.user) return;
      const userId = this.user.id;

      this.userService.restoreUser(userId).subscribe({
        next: (res: ResponseObject) => {
          Swal.fire('Thành công', res.message || 'Đã khôi phục tài khoản', 'success');
          this.loadUser(userId);
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể khôi phục tài khoản', 'error');
        },
      });
    });
  }
}
