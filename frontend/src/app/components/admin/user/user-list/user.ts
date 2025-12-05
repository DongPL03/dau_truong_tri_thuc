import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule} from '@angular/forms';
import Swal from 'sweetalert2';

import {Base} from '../../../base/base';
import {ResponseObject} from '../../../../responses/response-object';
import {UserListResponse} from '../../../../responses/nguoidung/user-list-response';
import {UserResponse} from '../../../../responses/nguoidung/user-response';
import {UserSummaryResponse} from '../../../../responses/nguoidung/user-summary-response';

@Component({
  selector: 'app-admin-user-list',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './user.html',
  styleUrl: './user.scss'
})
export class AdminUserList extends Base implements OnInit {

  loading = false;

  // filter
  keyword = '';
  statusFilter = ''; // 🔹 filter trạng thái

  // paging
  page = 0;
  limit = 10;
  totalPages = 0;

  // danh sách user
  allUsers: UserResponse[] = [];  // raw từ API
  users: UserResponse[] = [];     // sau khi áp dụng filter

  statusOptions = [
    {value: '', label: 'Tất cả'},
    {value: 'ACTIVE', label: 'Đang hoạt động'},
    {value: 'BLOCKED', label: 'Đã bị khoá'},
    {value: 'DELETED', label: 'Đã vô hiệu hoá'}
  ];


  ngOnInit(): void {
    this.loadUsers();
  }

  private buildParams() {
    return {
      page: this.page,
      limit: this.limit,
      keyword: this.keyword || ''
    };
  }

  loadUsers(): void {
    this.loading = true;
    this.userService.getUsers(this.buildParams()).subscribe({
      next: (res: ResponseObject<UserListResponse>) => {
        const data = res.data;
        this.allUsers = data?.users ?? [];
        this.totalPages = (data?.total_pages ?? 0);
        this.applyStatusFilter();
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải danh sách người dùng', 'error');
      }
    });
  }

  applyStatusFilter(): void {
    if (!this.statusFilter) {
      // Không filter → hiển thị tất cả
      this.users = [...this.allUsers];
      return;
    }

    this.users = this.allUsers.filter((u: any) => {
      // ⚠️ TUỲ cấu trúc UserResponse của bạn mà chỉnh lại điều kiện:
      // Giả sử: is_xoa: 0/1, is_active: 0/1
      switch (this.statusFilter) {
        case 'ACTIVE':
          return u.is_xoa === 0 && u.is_active !== 0;
        case 'BLOCKED':
          return u.is_xoa === 0 && u.is_active === 0;
        case 'DELETED':
          return u.is_xoa === 1;
        default:
          return true;
      }
    });
  }


  onSearch(): void {
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

  /** 🔐 Reset mật khẩu cho user */
  resetPassword(user: UserResponse): void {
    Swal.fire({
      title: 'Reset mật khẩu?',
      text: `User: ${user.ten_dang_nhap || user.email || user.ho_ten}`,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Reset',
      cancelButtonText: 'Huỷ'
    }).then(result => {
      if (!result.isConfirmed) return;

      this.userService.resetUserPassword(user.id).subscribe({
        next: (res: ResponseObject<string>) => {
          const newPass = res.data;
          Swal.fire(
            'Thành công',
            `Mật khẩu mới: <b>${newPass}</b>`,
            'success'
          ).then(r => {
          });
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể reset mật khẩu', 'error').then(r => {
          });
        }
      });
    });
  }

  /** 🚫 Khoá hoặc mở khoá user: active = false -> khoá, true -> mở */
  blockOrEnable(user: UserResponse, active: boolean): void {
    const actionText = active ? 'mở khoá' : 'khoá';
    Swal.fire({
      title: `Xác nhận ${actionText} tài khoản?`,
      text: `User: ${user.ten_dang_nhap || user.email || user.ho_ten}`,
      icon: 'warning',
      showCancelButton: true,
      confirmButtonText: 'Đồng ý',
      cancelButtonText: 'Huỷ'
    }).then(result => {
      if (!result.isConfirmed) return;

      this.userService.blockOrEnableUser(user.id, active).subscribe({
        next: (res: ResponseObject) => {
          Swal.fire('Thành công', res.message || 'Cập nhật trạng thái tài khoản thành công', 'success').then(r => {
          });
          this.loadUsers();
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể cập nhật trạng thái tài khoản', 'error').then(r => {
          });
        }
      });
    });
  }

  /** 👑 Đổi vai trò: 'ROLE_USER' / 'ROLE_ADMIN' */
  changeRole(user: UserResponse, role: string): void {
    const actionText = role === 'ADMIN' ? 'Nâng quyền ADMIN' : 'Chuyển về USER';
    console.log('Change role', user, role);
    Swal.fire({
      title: `${actionText}?`,
      text: `User: ${user.ten_dang_nhap || user.email || user.ho_ten}`,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Đồng ý',
      cancelButtonText: 'Huỷ'
    }).then(result => {
      if (!result.isConfirmed) return;

      this.userService.updateUserRole(user.id, role).subscribe({
        next: (res: ResponseObject) => {
          Swal.fire('Thành công', res.message || 'Cập nhật vai trò thành công', 'success').then(r => {
          });
          this.loadUsers();
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể cập nhật vai trò', 'error').then(r => {
          });
        }
      });
    });
  }

  /** ♻️ Khôi phục tài khoản đã deactivate / soft delete */
  restore(user: UserResponse): void {
    Swal.fire({
      title: 'Khôi phục tài khoản?',
      text: `User: ${user.ten_dang_nhap || user.email || user.ho_ten}`,
      icon: 'question',
      showCancelButton: true,
      confirmButtonText: 'Khôi phục',
      cancelButtonText: 'Huỷ'
    }).then(result => {
      if (!result.isConfirmed) return;

      this.userService.restoreUser(user.id).subscribe({
        next: (res: ResponseObject) => {
          Swal.fire('Thành công', res.message || 'Đã khôi phục tài khoản', 'success').then(r => {
          });
          this.loadUsers();
        },
        error: () => {
          Swal.fire('Lỗi', 'Không thể khôi phục tài khoản', 'error').then(r => {
          });
        }
      });
    });
  }

  /** 📊 Xem tổng quan user (dữ liệu từ BXH) */
  viewSummary(user: UserResponse): void {
    this.userService.getUserSummary(user.id).subscribe({
      next: (res: ResponseObject<UserSummaryResponse>) => {
        const summary = res.data;
        if (!summary) {
          Swal.fire('Thông báo', 'Không có dữ liệu tổng quan cho người dùng này', 'info').then(r => {
          });
          return;
        }

        // Hiển thị raw JSON cho chắc ăn, tránh lệch field,
        // sau bạn muốn đẹp hơn thì map từng trường ra.
        const pretty = JSON.stringify(summary, null, 2);
        const html = `
          <div style="text-align:left; max-height:400px; overflow:auto;">
            <pre style="white-space:pre-wrap;">${pretty}</pre>
          </div>
        `;

        Swal.fire({
          title: `Tổng quan người dùng #${user.id}`,
          html,
          icon: 'info',
          width: 600
        }).then(r => {
        });
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể lấy thông tin tổng quan người dùng', 'error').then(r => {
        });
      }
    });
  }

  viewDetail(user: UserResponse): void {
    this.router.navigate(['/admin/users', user.id], {
      state: {user}
    }).then(r => {
    });
  }


}
