import {Component, OnInit, ViewChild} from '@angular/core';
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
import {AchievementResponse} from '../../responses/thanhtich/achievement-response';
import {UserSummaryResponse} from '../../responses/nguoidung/user-summary-response';
import {LichSuTranDauResponse as LichSuTranDauShort} from '../../responses/lichsutrandau/lich_su_tran_dau_response';
import {TopicStatResponse} from '../../responses/thongke/topic-stat-response';


@Component({
  selector: 'app-profile',
  standalone: true,
  imports: [CommonModule, FormsModule, RouterLink, AddressSelector],
  templateUrl: './profile.html',
  styleUrl: './profile.scss',
})
export class Profile extends Base implements OnInit {
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

  // Tab hiện tại: INFO = thông tin cá nhân, ACHIEVEMENTS = thành tích & tiến trình
  active_tab: 'INFO' | 'ACHIEVEMENTS' | 'ANALYSIS' = 'INFO';

  // Thành tích
  achievements: AchievementResponse[] = [];
  loading_achievements = false;

  // Chỗ chứa exp/level/rank/gold & thống kê trận (lấy từ API mà bạn đã dùng ở trang home)
  me_rank: any = null;

  user?: UserResponse | null;

  user_summary?: UserSummaryResponse | null;
  recent_matches: LichSuTranDauShort[] = [];
  loading_summary = false;

  topic_stats: TopicStatResponse[] = [];
  loading_topics = false;

  ngOnInit(): void {
    this.me = this.userService.getUserResponseFromLocalStorage();
    this.fetchProfile();
    this.user = this.userService.getUserResponseFromLocalStorage();

    if (this.user?.id) {
      this.loadUserSummary(this.user.id);
    }
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
            location.href = '/dang-nhap';
          }
        });
      },
    });
  }

  /** Chuyển tab */
  setTab(tab: 'INFO' | 'ACHIEVEMENTS' | 'ANALYSIS'): void {
    this.active_tab = tab;
    if (tab === 'ACHIEVEMENTS' && !this.loading_achievements && this.achievements.length === 0) {
      this.loadAchievements();
    }

    if (tab === 'ANALYSIS' && this.topic_stats.length === 0) {
      this.loadTopicStats();
    }
  }

  /** Lấy danh sách thành tích của user */
  loadAchievements(): void {
    this.loading_achievements = true;

    // Gợi ý: userService.getMyAchievements() gọi về /achievements/me
    this.thanhTichService.getMyAchievements().subscribe({
      next: (res: ResponseObject) => {
        this.achievements = (res.data || []) as AchievementResponse[];
        this.loading_achievements = false;
      },
      error: () => {
        this.loading_achievements = false;
        // Có thể show toast nhẹ nếu muốn
        // this.toastService.show('Không thể tải thành tích', 'error');
      }
    });
  }

  /** Lấy exp/level/rank/gold + tổng trận, thắng/thua (từ BXH) */
  loadUserSummary(user_id: number) {
    this.loading_summary = true;

    this.userService.getUserSummary(user_id).subscribe({
      next: (res: ResponseObject<UserSummaryResponse>) => {
        console.log('✅ Thống kê người dùng tải về:', res.data);
        this.loading_summary = false;
        this.user_summary = res.data ?? null;
        console.log('User Summary:', this.user_summary);

        // Lấy tối đa 3 trận gần nhất từ lich_su_tran_dau (nếu backend có trả)
        const history = this.user_summary?.lich_su_tran_dau ?? [];
        this.recent_matches = history.slice(0, 3);
      },
      error: (err) => {
        this.loading_summary = false;
        console.error('❌ Lỗi khi tải thống kê người dùng:', err);
      }
    });
  }

  /** Gọi API thống kê mạnh/yếu theo chủ đề */
  private loadTopicStats(): void {
    this.loading_topics = true;

    this.userStatsService.getMyTopicStats().subscribe({
      next: (res: ResponseObject) => {
        this.topic_stats = (res.data || []) as TopicStatResponse[];
        this.loading_topics = false;
      },
      error: () => {
        this.loading_topics = false;
        Swal.fire('Lỗi', 'Không thể tải thống kê theo chủ đề', 'error').then(() => {
        });
      }
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
            location.href = '/dang-nhap';
          }
        });
      },
      complete: () => {
        this.changingPass = false;
        this.tokenService.clear();
        this.userService.removeUserFromLocalStorage();
        location.href = '/dang-nhap';
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
