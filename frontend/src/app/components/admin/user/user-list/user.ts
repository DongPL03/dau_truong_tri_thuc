import {CommonModule} from '@angular/common';
import {Component, OnInit} from '@angular/core';
import {FormsModule} from '@angular/forms';
import Swal from 'sweetalert2';

import {UserListResponse} from '../../../../responses/nguoidung/user-list-response';
import {UserResponse} from '../../../../responses/nguoidung/user-response';
import {UserSummaryResponse} from '../../../../responses/nguoidung/user-summary-response';
import {ResponseObject} from '../../../../responses/response-object';
import {Base} from '../../../base/base';
import {ClickOutsideDirective} from '../../../notification-bell/click-outside.directive';
import { NgbDropdownModule } from '@ng-bootstrap/ng-bootstrap';

@Component({
  selector: 'app-admin-user-list',
  standalone: true,
  imports: [CommonModule, FormsModule, NgbDropdownModule],
  templateUrl: './user.html',
  styleUrl: './user.scss',
})
export class AdminUserList extends Base implements OnInit {
  // ===================== THỐNG KÊ =====================
  stats = {
    totalUsers: 0,
    activeUsers: 0,
    blockedUsers: 0,
    deletedUsers: 0,
    totalAdmins: 0,
    todayRegistrations: 0,
  };
  loadingStats = false;

  // ===================== DANH SÁCH USER =====================
  loading = false;

  // filter
  keyword = '';
  statusFilter = '';

  // paging
  page = 0;
  limit = 10;
  totalPages = 0;

  // danh sách user
  allUsers: UserResponse[] = [];
  users: UserResponse[] = [];

  // Current logged in admin ID
  currentAdminId: number = 0;

  // Modal xem tổng quan
  showSummaryModal = false;
  summaryLoading = false;
  selectedUserSummary: UserSummaryResponse | null = null;
  selectedUser: UserResponse | null = null;

  statusOptions = [
    {value: '', label: 'Tất cả trạng thái'},
    {value: 'ACTIVE', label: 'Đang hoạt động'},
    {value: 'BLOCKED', label: 'Đã bị khoá'},
    {value: 'DELETED', label: 'Đã vô hiệu hoá'},
  ];

  ngOnInit(): void {
    this.currentAdminId = this.tokenService.getUserId();
    this.loadStats();
    this.loadUsers();
  }

  // ===================== THỐNG KÊ =====================
  loadStats(): void {
    this.loadingStats = true;
    this.userService.getAdminUserStats().subscribe({
      next: (res: ResponseObject<any>) => {
        this.stats = res.data ?? this.stats;
        this.loadingStats = false;
      },
      error: () => {
        this.loadingStats = false;
      },
    });
  }

  // ===================== DANH SÁCH USER =====================
  private buildParams() {
    return {
      page: this.page,
      limit: this.limit,
      keyword: this.keyword || '',
    };
  }

  loadUsers(): void {
    this.loading = true;
    this.userService.getAdminUserList(this.buildParams()).subscribe({
      next: (res: ResponseObject<UserListResponse>) => {
        const data = res.data;
        console.log(data);
        this.allUsers = data?.users ?? [];
        this.totalPages = data?.total_pages ?? 0;
        this.applyStatusFilter();
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải danh sách người dùng', 'error');
      },
    });
  }

  applyStatusFilter(): void {
    if (!this.statusFilter) {
      this.users = [...this.allUsers];
      return;
    }

    this.users = this.allUsers.filter((u: any) => {
      switch (this.statusFilter) {
        case 'ACTIVE':
          return u.is_xoa === false && u.is_active !== false;
        case 'BLOCKED':
          return u.is_xoa === false && u.is_active === false;
        case 'DELETED':
          return u.is_xoa === true;
        default:
          return true;
      }
    });
  }

  onSearch(): void {
    this.page = 0;
    this.loadUsers();
  }

  clearFilters(): void {
    this.keyword = '';
    this.statusFilter = '';
    this.page = 0;
    this.loadUsers();
  }

  changePage(newPage: number): void {
    if (newPage < 0 || newPage >= this.totalPages) {
      return;
    }
    this.page = newPage;
    this.loadUsers();
  }

  getPageNumbers(): number[] {
    const pages: number[] = [];
    const maxVisible = 5;
    let start = Math.max(0, this.page - Math.floor(maxVisible / 2));
    let end = Math.min(this.totalPages, start + maxVisible);

    if (end - start < maxVisible) {
      start = Math.max(0, end - maxVisible);
    }

    for (let i = start; i < end; i++) {
      pages.push(i);
    }
    return pages;
  }

  getUserStatus(user: any): { label: string; class: string } {
    if (user.is_xoa === 1) {
      return {label: 'Đã xóa', class: 'status-deleted'};
    }
    if (user.is_active === 0) {
      return {label: 'Bị khóa', class: 'status-blocked'};
    }
    return {label: 'Hoạt động', class: 'status-active'};
  }

  // ===================== ACTIONS =====================

  /** 🔐 Reset mật khẩu cho user */
  resetPassword(user: UserResponse): void {
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

  /** 🚫 Khoá hoặc mở khoá user */
  blockOrEnable(user: UserResponse, active: boolean): void {
    if (user.id === this.currentAdminId && !active) {
      Swal.fire('Cảnh báo', 'Bạn không thể khóa chính tài khoản của mình', 'warning');
      return;
    }

    const actionText = active ? 'mở khoá' : 'khoá';
    Swal.fire({
      title: `Xác nhận ${actionText} tài khoản?`,
      text: `User: ${user.ten_dang_nhap || user.email || user.ho_ten}`,
      icon: 'warning',
      showCancelButton: true,
      confirmButtonColor: active ? '#28a745' : '#d33',
      confirmButtonText: 'Đồng ý',
      cancelButtonText: 'Huỷ',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.userService.blockOrEnableUser(user.id, active).subscribe({
        next: (res: ResponseObject) => {
          Swal.fire('Thành công', res.message || 'Cập nhật trạng thái thành công', 'success');
          this.loadUsers();
          this.loadStats();
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể cập nhật trạng thái tài khoản', 'error');
        },
      });
    });
  }

  /** 👑 Đổi vai trò */
  changeRole(user: UserResponse, role: string): void {
    if (user.id === this.currentAdminId) {
      Swal.fire('Cảnh báo', 'Bạn không thể thay đổi vai trò của chính mình', 'warning');
      return;
    }

    const actionText = role === 'ADMIN' ? 'Nâng quyền ADMIN' : 'Chuyển về USER';
    Swal.fire({
      title: `${actionText}?`,
      text: `User: ${user.ten_dang_nhap || user.email || user.ho_ten}`,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Đồng ý',
      cancelButtonText: 'Huỷ',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.userService.updateUserRole(user.id, role).subscribe({
        next: (res: ResponseObject) => {
          Swal.fire('Thành công', res.message || 'Cập nhật vai trò thành công', 'success');
          this.loadUsers();
          this.loadStats();
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể cập nhật vai trò', 'error');
        },
      });
    });
  }

  /** 🗑️ Xóa mềm user */
  softDelete(user: UserResponse): void {
    if (user.id === this.currentAdminId) {
      Swal.fire('Cảnh báo', 'Bạn không thể xóa chính tài khoản của mình', 'warning');
      return;
    }

    Swal.fire({
      title: 'Xác nhận xóa tài khoản?',
      text: `User: ${
        user.ten_dang_nhap || user.email || user.ho_ten
      }. Tài khoản sẽ bị vô hiệu hóa.`,
      icon: 'warning',
      showCancelButton: true,
      confirmButtonColor: '#d33',
      confirmButtonText: 'Xóa',
      cancelButtonText: 'Huỷ',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.userService.adminSoftDeleteUser(user.id).subscribe({
        next: () => {
          Swal.fire('Thành công', 'Đã xóa tài khoản', 'success');
          this.loadUsers();
          this.loadStats();
        },
        error: (err) => {
          Swal.fire('Lỗi', err.error?.message || 'Không thể xóa tài khoản', 'error');
        },
      });
    });
  }

  /** ♻️ Khôi phục tài khoản */
  restore(user: UserResponse): void {
    Swal.fire({
      title: 'Khôi phục tài khoản?',
      text: `User: ${user.ten_dang_nhap || user.email || user.ho_ten}`,
      icon: 'question',
      showCancelButton: true,
      confirmButtonColor: '#28a745',
      confirmButtonText: 'Khôi phục',
      cancelButtonText: 'Huỷ',
    }).then((result) => {
      if (!result.isConfirmed) return;

      this.userService.restoreUser(user.id).subscribe({
        next: (res: ResponseObject) => {
          Swal.fire('Thành công', res.message || 'Đã khôi phục tài khoản', 'success');
          this.loadUsers();
          this.loadStats();
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể khôi phục tài khoản', 'error');
        },
      });
    });
  }

  // ===================== MODAL TỔNG QUAN =====================
  viewSummary(user: UserResponse): void {
    this.selectedUser = user;
    this.showSummaryModal = true;
    this.summaryLoading = true;
    this.selectedUserSummary = null;

    this.userService.getUserSummary(user.id).subscribe({
      next: (res: ResponseObject<UserSummaryResponse>) => {
        this.selectedUserSummary = res.data!;
        this.summaryLoading = false;
      },
      error: () => {
        this.summaryLoading = false;
        Swal.fire('Lỗi', 'Không thể lấy thông tin tổng quan', 'error');
        this.closeSummaryModal();
      },
    });
  }

  closeSummaryModal(): void {
    this.showSummaryModal = false;
    this.selectedUser = null;
    this.selectedUserSummary = null;
  }

  viewDetail(user: UserResponse): void {
    this.router.navigate(['/admin/users', user.id], {state: {user}});
  }

  // ===================== EXPORT CSV =====================
  exportCsv(): void {
    this.userService.adminExportUsersCsv(this.keyword || undefined).subscribe({
      next: (blob: Blob) => {
        const url = window.URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = `users_export_${new Date().toISOString().split('T')[0]}.csv`;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        window.URL.revokeObjectURL(url);
        Swal.fire('Thành công', 'Đã xuất file CSV', 'success');
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể xuất file CSV', 'error');
      },
    });
  }
  // Hàm tính tỉ lệ thắng cho Modal (nếu chưa có)
  calculateWinRate(summary: any): number {
    if (!summary || summary.tong_tran === 0) return 0;
    return Math.round((summary.so_tran_thang / summary.tong_tran) * 100);
  }
}
