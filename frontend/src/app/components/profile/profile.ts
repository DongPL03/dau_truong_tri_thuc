import {Component, ViewChild} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule, NgForm} from '@angular/forms';
import {HttpErrorResponse} from '@angular/common/http';
import Swal from 'sweetalert2';
import {RouterLink} from '@angular/router';
import {Base} from '../base/base';
import {UserResponse} from '../../responses/nguoidung/user-response';
import {ResponseObject} from '../../responses/response-object';
import {UpdateUserDTO} from '../../dtos/nguoi-dung/update-user-dto';
import {environment} from '../../environments/environment';
import {AddressSelector} from '../shared/address-selector/address-selector';


@Component({
  selector: 'app-profile',
  standalone: true,
  imports: [CommonModule, FormsModule, RouterLink, AddressSelector],
  templateUrl: './profile.html',
  styleUrl: './profile.scss',
})
export class Profile extends Base {
  @ViewChild('infoForm') infoForm!: NgForm;
  @ViewChild('passwordForm') passwordForm!: NgForm;

  me?: UserResponse | null;
  avatarPreview?: string;
  uploading = false;
  loading = true;
  saving = false;
  changingPass = false;

  // Base URL để hiển thị ảnh
  private readonly imageBaseUrl = `${environment.apiBaseUrl}/users/profile-images/`;

  updateDto: UpdateUserDTO = new UpdateUserDTO();
  passwordModel = {oldPassword: '', newPassword: '', retypePassword: ''};

  ngOnInit(): void {
    this.me = this.userService.getUserResponseFromLocalStorage();
    this.fetchProfile();
  }

  /** Lấy thông tin hồ sơ */
  fetchProfile(): void {
    this.loading = true;
    this.userService.getUserDetails().subscribe({
      next: (res: ResponseObject) => {
        this.me = res.data as UserResponse;
        this.userService.saveUserResponseToLocalStorage(this.me);

        this.updateDto = {
          ho_ten: this.me?.ho_ten?.trim() ?? '',
          ten_hien_thi: this.me?.ten_hien_thi?.trim() ?? '',
          email: this.me?.email?.trim() ?? '',
          dia_chi: this.me?.dia_chi?.trim() ?? '',
        };

        this.avatarPreview = this.me?.avatar_url
          ? this.imageBaseUrl + this.me.avatar_url
          : undefined;
        this.loading = false;
      },
      error: (err: HttpErrorResponse) => {
        this.loading = false;
        Swal.fire('Lỗi', err.error?.message || 'Không thể tải thông tin người dùng', 'error').then(r => {
          if (r.isConfirmed) {
            this.tokenService.clear();
            this.userService.removeUserFromLocalStorage();
            location.href = '/login';
          }
        });
      },
    });
  }

  /** Lưu thông tin cá nhân */
  onSaveInfo(form: NgForm): void {
    if (form.invalid || !this.me) {
      Swal.fire('Cảnh báo', 'Vui lòng điền đầy đủ thông tin hợp lệ', 'warning').then(r => {
        if (r.isConfirmed) {
          this.infoForm.control.markAllAsTouched();
        }
      });
      return;
    }

    this.saving = true;
    this.userService.updateMe(this.me.id!, this.updateDto).subscribe({
      next: (res: ResponseObject) => {
        this.me = res.data as UserResponse;
        this.userService.saveUserResponseToLocalStorage(this.me);
        Swal.fire('Thành công', 'Cập nhật thông tin thành công', 'success').then(r => {
          if (r.isConfirmed) {
            this.fetchProfile();
          }
        });
      },
      complete: () => {
        this.saving = false;
      },
      error: (err: HttpErrorResponse) => {
        Swal.fire('Lỗi', err.error?.message || 'Không thể cập nhật thông tin', 'error').then(r => {
          if (r.isConfirmed) {
            this.infoForm.control.markAllAsTouched();
          }
        });
        this.saving = false;
      },
    });
  }

  /** Đổi mật khẩu */
  onChangePassword(form: NgForm): void {
    const {oldPassword, newPassword, retypePassword} = this.passwordModel;
    if (!oldPassword || !newPassword) {
      Swal.fire('Cảnh báo', 'Vui lòng nhập đầy đủ mật khẩu', 'warning').then(r => {
        if (r.isConfirmed) {
          this.passwordForm.control.markAllAsTouched();
        }
      });
      return;
    }
    if (newPassword.length < 6) {
      Swal.fire('Cảnh báo', 'Mật khẩu mới phải có ít nhất 6 ký tự', 'warning').then(r => {
        if (r.isConfirmed) {
          this.passwordForm.control.markAllAsTouched();
        }
      });
      return;
    }
    if (newPassword !== retypePassword) {
      Swal.fire('Cảnh báo', 'Mật khẩu nhập lại không khớp', 'warning').then(r => {
        if (r.isConfirmed) {
          this.passwordForm.control.markAllAsTouched();
        }
      });
      return;
    }

    this.changingPass = true;
    this.userService.changePassword(oldPassword, newPassword).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Đổi mật khẩu thành công, vui lòng đăng nhập lại', 'success').then(r => {
          if (r.isConfirmed) {
            this.tokenService.clear();
            this.userService.removeUserFromLocalStorage();
            location.href = '/login';
          }
        });
      },
      complete: () => {
        this.changingPass = false;
        this.tokenService.clear();
        this.userService.removeUserFromLocalStorage();
        location.href = '/login';
      },
      error: (err: HttpErrorResponse) => {
        Swal.fire('Lỗi', err.error?.message || 'Không thể đổi mật khẩu', 'error').then(r => {
          if (r.isConfirmed) {
            this.passwordForm.control.markAllAsTouched();
          }
        });
        this.changingPass = false;
      },
    });
  }

  /** Upload ảnh đại diện */
  onAvatarSelected(event: Event): void {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;
    const file = input.files[0];

    const reader = new FileReader();
    reader.onload = () => (this.avatarPreview = reader.result as string);
    reader.readAsDataURL(file);

    this.uploading = true;
    this.userService.uploadProfileImage(file).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Cập nhật ảnh đại diện thành công', 'success').then(r => {
          if (r.isConfirmed) {
            this.uploading = false;
            this.fetchProfile();
          }
        });
      },
      complete: () => {
        this.uploading = false;
        this.fetchProfile();
      },
      error: (err: HttpErrorResponse) => {
        Swal.fire('Lỗi', err.error?.message || 'Tải ảnh thất bại', 'error').then(r => {
          if (r.isConfirmed) {
            this.fetchProfile();
          }
        });
        this.uploading = false;
      },
    });
  }
}
