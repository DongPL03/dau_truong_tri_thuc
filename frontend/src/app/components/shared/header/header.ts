import {Component, HostListener, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import {RouterLink, RouterLinkActive} from '@angular/router';
import {UserResponse} from '../../../responses/nguoidung/user-response';
import {Base} from '../../base/base';
import {NotificationBell} from '../../notification-bell/notification-bell';

/**
 * 🔹 Header Component
 * Hiển thị thanh điều hướng, avatar, menu người dùng, và thông báo.
 */
@Component({
  selector: 'app-header',
  standalone: true,
  imports: [CommonModule, RouterLink, RouterLinkActive, NotificationBell],
  templateUrl: './header.html',
  styleUrls: ['./header.scss'],
})
export class Header extends Base implements OnInit {
  user?: UserResponse | null = null;
  avatarUrl: string = 'assets/images/default-profile-image.jpeg';
  notifOpen = false;
  userMenuOpen = false;
  unreadCount = 3;

  readonly imageBaseUrl = 'http://localhost:8088/api/v1/users/profile-images/';

  /** 🔔 Danh sách thông báo (demo) */
  notifications = [
    {id: 1, icon: 'fas fa-bolt', text: 'Nguyễn Văn A đã mời bạn vào trận đấu ⚡'},
    {id: 2, icon: 'fas fa-user-plus', text: 'Trần Thị Nhi đã chấp nhận lời mời kết bạn 🤝'},
    {id: 3, icon: 'fas fa-trophy', text: 'Bạn đã thắng trận "Lịch sử Việt Nam" 🏆'},
  ];


  ngOnInit(): void {
    this.loadUserInfo();
  }

  /** 🧩 Lấy dữ liệu người dùng từ LocalStorage */
  private loadUserInfo(): void {
    this.user = this.userService.getUserResponseFromLocalStorage();

    if (this.user?.avatar_url) {
      this.avatarUrl = this.imageBaseUrl + this.user.avatar_url;
    } else {
      this.avatarUrl = 'assets/images/default-profile-image.jpeg';
    }
  }

  /** 🔔 Toggle dropdown thông báo */
  toggleNotif(): void {
    this.notifOpen = !this.notifOpen;
    if (this.notifOpen) {
      this.userMenuOpen = false;
      this.unreadCount = 0;
    }
  }

  /** 👤 Toggle dropdown menu người dùng */
  toggleUserMenu(): void {
    this.userMenuOpen = !this.userMenuOpen;
    if (this.userMenuOpen) this.notifOpen = false;
  }

  /** 🧭 Điều hướng đến hồ sơ cá nhân */
  goProfile(): void {
    this.router.navigate(['/profile']).then();
  }

  /** 🚪 Đăng xuất người dùng */
  logout(): void {
    // Gọi backend logout để set OFFLINE + revoke token
    this.userService.logoutBackend().subscribe({
      next: (res) => {
        console.log('Logout backend ok:', res);
        this.afterLogout();
      },
      error: (err) => {
        console.error('Logout backend error:', err);
        // Dù lỗi (ví dụ token hết hạn) vẫn nên clear FE để tránh kẹt user
        this.afterLogout();
      }
    });
  }

// Tách phần clear local state riêng cho gọn
  private afterLogout(): void {
    this.tokenService.clear();
    this.userService.removeUserFromLocalStorage();
    this.router.navigate(['/login']).then(() => {
      setTimeout(() => location.reload(), 200);
    });
  }


  /** 🚫 Ẩn các dropdown khi click ra ngoài */
  @HostListener('document:click')
  onOutsideClick(): void {
    this.notifOpen = false;
    this.userMenuOpen = false;
  }
}
