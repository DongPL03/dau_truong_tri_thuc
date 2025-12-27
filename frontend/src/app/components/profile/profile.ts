import {CommonModule} from '@angular/common';
import {HttpErrorResponse} from '@angular/common/http';
import {Component, OnInit, ViewChild} from '@angular/core';
import {FormsModule, NgForm} from '@angular/forms';
import {RouterLink} from '@angular/router';
import Swal from 'sweetalert2';
import {UpdateUserDTO} from '../../dtos/nguoi-dung/update-user-dto';
import {environment} from '../../environments/environment';
import {CourseStatResponse} from '../../responses/khoahoc/course-stat-response';
import {LichSuTranDauResponse as LichSuTranDauShort} from '../../responses/lichsutrandau/lich_su_tran_dau_response';
import {UserResponse} from '../../responses/nguoidung/user-response';
import {UserSummaryResponse} from '../../responses/nguoidung/user-summary-response';
import {ResponseObject} from '../../responses/response-object';
import {AchievementResponse} from '../../responses/thanhtich/achievement-response';
import {TopicStatResponse} from '../../responses/thongke/topic-stat-response';
import {Base} from '../base/base';
import {AddressSelector} from '../shared/address-selector/address-selector';

@Component({
  selector: 'app-profile',
  standalone: true,
  imports: [CommonModule, FormsModule, AddressSelector],
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

  private readonly imageBaseUrl = `${environment.apiBaseUrl}/users/profile-images/`;

  updateDto: UpdateUserDTO = new UpdateUserDTO();
  passwordModel = {oldPassword: '', newPassword: '', retypePassword: ''};

  active_tab: 'INFO' | 'ACHIEVEMENTS' | 'ANALYSIS' = 'INFO';

  // Stats Data
  achievements: AchievementResponse[] = [];
  loading_achievements = false;

  user_summary?: UserSummaryResponse | null;
  recent_matches: LichSuTranDauShort[] = [];
  loading_summary = false;

  topic_stats: TopicStatResponse[] = [];
  loading_topics = false;

  course_stats?: CourseStatResponse | null;
  loading_course_stats = false;

  // Rank Info (Có thể gộp vào user_summary hoặc lấy riêng)
  me_rank: any = {rank_tier: 'Tập sự'}; // Giá trị mặc định an toàn

  ngOnInit(): void {
    // Lấy user từ local storage trước để render nhanh
    this.me = this.userService.getUserResponseFromLocalStorage();

    // Fetch dữ liệu mới nhất
    this.fetchProfile();

    // Nếu có ID, load thêm thống kê rank/level
    if (this.me?.id) {
      this.loadUserSummary(this.me.id);
    }
  }

  fetchProfile(): void {
    this.loading = true;
    this.userService.getUserDetails().subscribe({
      next: (res: ResponseObject) => {
        this.me = res.data as UserResponse;
        this.userService.saveUserResponseToLocalStorage(this.me);

        // Map data sang DTO để binding form
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
        console.error('Error fetching profile:', err);
      },
    });
  }

  setTab(tab: 'INFO' | 'ACHIEVEMENTS' | 'ANALYSIS'): void {
    this.active_tab = tab;

    // Lazy load dữ liệu khi bấm tab
    if (tab === 'ACHIEVEMENTS') {
      if (!this.achievements.length) this.loadAchievements();
      if (!this.course_stats) this.loadCourseStats();
    }
    if (tab === 'ANALYSIS' && !this.topic_stats.length) {
      this.loadTopicStats();
    }
  }

  loadAchievements(): void {
    this.loading_achievements = true;
    this.thanhTichService.getMyAchievements().subscribe({
      next: (res: ResponseObject) => {
        this.achievements = (res.data || []) as AchievementResponse[];
        this.loading_achievements = false;
      },
      error: () => this.loading_achievements = false
    });
  }

  loadUserSummary(user_id: number) {
    this.loading_summary = true;
    this.userService.getUserSummary(user_id).subscribe({
      next: (res: ResponseObject<UserSummaryResponse>) => {
        this.loading_summary = false;
        this.user_summary = res.data ?? null;
        this.me_rank = this.user_summary; // Map sang biến me_rank để dùng chung logic
      },
      error: () => this.loading_summary = false
    });
  }

  private loadTopicStats(): void {
    this.loading_topics = true;
    this.userStatsService.getMyTopicStats().subscribe({
      next: (res: ResponseObject) => {
        this.topic_stats = (res.data || []) as TopicStatResponse[];
        this.loading_topics = false;
      },
      error: () => this.loading_topics = false
    });
  }

  private loadCourseStats(): void {
    this.loading_course_stats = true;
    this.khoaHocService.getCourseStats().subscribe({
      next: (res: ResponseObject<CourseStatResponse>) => {
        this.course_stats = res.data ?? null;
        this.loading_course_stats = false;
      },
      error: () => this.loading_course_stats = false
    });
  }

  onSaveInfo(form: NgForm): void {
    if (form.invalid || !this.me) {
      Swal.fire('Thiếu thông tin', 'Vui lòng kiểm tra lại các trường.', 'warning');
      return;
    }

    this.saving = true;
    this.userService.updateMe(this.me.id!, this.updateDto).subscribe({
      next: (res: ResponseObject) => {
        this.me = res.data as UserResponse;
        this.userService.saveUserResponseToLocalStorage(this.me);
        Swal.fire('Thành công', 'Cập nhật hồ sơ thành công!', 'success');
        this.saving = false;
      },
      error: (err: HttpErrorResponse) => {
        Swal.fire('Lỗi', err.error?.message || 'Cập nhật thất bại', 'error');
        this.saving = false;
      }
    });
  }

  onChangePassword(form: NgForm): void {
    const {oldPassword, newPassword, retypePassword} = this.passwordModel;
    if (!oldPassword || !newPassword) {
      Swal.fire('Lỗi', 'Vui lòng nhập đầy đủ mật khẩu', 'warning');
      return;
    }
    if (newPassword !== retypePassword) {
      Swal.fire('Lỗi', 'Mật khẩu xác nhận không khớp', 'warning');
      return;
    }

    this.changingPass = true;
    this.userService.changePassword(oldPassword, newPassword).subscribe({
      next: () => {
        Swal.fire('Thành công', 'Đổi mật khẩu thành công. Vui lòng đăng nhập lại.', 'success').then(() => {
          this.tokenService.clear();
          this.userService.removeUserFromLocalStorage();
          location.href = '/dang-nhap';
        });
      },
      error: (err: HttpErrorResponse) => {
        Swal.fire('Lỗi', err.error?.message || 'Đổi mật khẩu thất bại', 'error');
        this.changingPass = false;
      }
    });
  }

  onAvatarSelected(event: Event): void {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;
    const file = input.files[0];

    // Preview
    const reader = new FileReader();
    reader.onload = () => (this.avatarPreview = reader.result as string);
    reader.readAsDataURL(file);

    // Upload
    this.uploading = true;
    this.userService.uploadProfileImage(file).subscribe({
      next: () => {
        this.uploading = false;
        Swal.fire({
          icon: 'success',
          title: 'Cập nhật ảnh',
          text: 'Ảnh đại diện đã được thay đổi!',
          timer: 1500,
          showConfirmButton: false
        });
        this.fetchProfile(); // Refresh để lấy URL chuẩn từ server
      },
      error: () => {
        this.uploading = false;
        Swal.fire('Lỗi', 'Không thể tải ảnh lên', 'error');
      }
    });
  }
}
