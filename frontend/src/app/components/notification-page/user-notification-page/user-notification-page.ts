import {CommonModule} from '@angular/common';
import {Component, OnInit} from '@angular/core';
import {RouterModule} from '@angular/router'; // Thêm Router
import {NotificationResponse} from '../../../responses/notification/notification-response';
import {PageResponse} from '../../../responses/page-response';
import {ResponseObject} from '../../../responses/response-object';
import {Base} from '../../base/base';

@Component({
  selector: 'app-user-notification-page',
  standalone: true,
  imports: [CommonModule, RouterModule],
  templateUrl: './user-notification-page.html',
  styleUrl: './user-notification-page.scss',
})
export class UserNotificationPage extends Base implements OnInit {
  notifications: NotificationResponse[] = [];
  loading = false;

  page = 0;
  limit = 10;
  total_pages = 0;

  only_unread = false;

  ngOnInit(): void {
    this.loadPage(0);
  }

  loadPage(page: number): void {
    if (page < 0) return;
    this.loading = true;

    // Lưu ý: Logic lọc client-side này chỉ tốt cho dữ liệu ít.
    // Nếu dữ liệu nhiều, nên gọi API backend có param ?unread=true
    this.notificationService.getMy(page, this.limit).subscribe({
      next: (res: ResponseObject<PageResponse<NotificationResponse>>) => {
        const data = res.data;
        if (data) {
          let list = data.items || [];
          // Client-side filter (Tạm thời)
          if (this.only_unread) {
            list = list.filter((n: NotificationResponse) => !n.da_doc);
          }
          this.notifications = list;
          this.page = data.currentPage;
          this.total_pages = data.totalPages;
        }
        this.loading = false;
      },
      error: () => this.loading = false,
    });
  }

  setFilter(isUnread: boolean) {
    this.only_unread = isUnread;
    this.loadPage(0);
  }

  markAllRead() {
    this.notificationService.markAllAsRead().subscribe(() => {
      this.notifications.forEach(n => n.da_doc = true);
      // Nếu đang ở tab chưa đọc thì reload lại list
      if (this.only_unread) this.loadPage(0);
    });
  }

  markOneRead(event: Event, item: NotificationResponse) {
    event.stopPropagation();
    this.notificationService.markAsRead(item.thong_bao_id).subscribe(() => {
      item.da_doc = true;
      if (this.only_unread) {
        this.notifications = this.notifications.filter(n => n.thong_bao_id !== item.thong_bao_id);
      }
    });
  }

  deleteNotification(event: Event, item: NotificationResponse) {
    event.stopPropagation();
    // Gọi API xóa (Giả sử có hàm delete)
    // this.notificationService.delete(item.thong_bao_id).subscribe(...)
    this.notifications = this.notifications.filter(n => n.thong_bao_id !== item.thong_bao_id);
  }

  onClickItem(item: NotificationResponse) {
    if (!item.da_doc) {
      this.notificationService.markAsRead(item.thong_bao_id).subscribe(() => item.da_doc = true);
    }

    // Điều hướng (Logic giống component chuông)
    if (this.isQuizNotification(item)) {
      this.goToQuiz(null, item);
    } else if (this.getTypeFromMetadata(item) === 'FRIEND_REQUEST') {
      this.router.navigate(['/ban-be']);
    } else if (this.getTypeFromMetadata(item) === 'BATTLE_INVITE') {
      // Logic vào trận...
    }
  }

  prevPage(): void {
    if (this.page > 0) this.loadPage(this.page - 1);
  }

  nextPage(): void {
    if (this.page + 1 < this.total_pages) this.loadPage(this.page + 1);
  }

  // --- HELPER UI ---
  getIconClass(n: any): string {
    const t = this.getTypeFromMetadata(n);
    if (t === 'FRIEND_REQUEST') return 'friend';
    if (t === 'BATTLE_INVITE') return 'battle';
    if (t === 'QUIZ_APPROVED' || t === 'QUIZ_UNLOCKED') return 'gold';
    return 'system';
  }

  getIcon(n: any): string {
    const t = this.getTypeFromMetadata(n);
    if (t === 'FRIEND_REQUEST') return 'fas fa-user-plus';
    if (t === 'BATTLE_INVITE') return 'fas fa-swords';
    if (t === 'QUIZ_APPROVED') return 'fas fa-check-circle';
    if (t === 'QUIZ_UNLOCKED') return 'fas fa-unlock-alt';
    return 'fas fa-bell';
  }

  private getTypeFromMetadata(n: NotificationResponse): string {
    if (n.metadata) {
      try {
        return JSON.parse(n.metadata)?.type || n.loai;
      } catch {
      }
    }
    return n.loai;
  }

  getGoldReward(n: NotificationResponse): number | null {
    if (!n.metadata) return null;
    try {
      return JSON.parse(n.metadata)?.gold_reward || null;
    } catch {
      return null;
    }
  }

  isQuizNotification(n: NotificationResponse): boolean {
    const t = this.getTypeFromMetadata(n);
    return t === 'QUIZ_APPROVED' || t === 'QUIZ_UNLOCKED';
  }

  getQuizId(n: NotificationResponse): number | null {
    if (!n.metadata) return null;
    try {
      return JSON.parse(n.metadata)?.bo_cau_hoi_id || null;
    } catch {
      return null;
    }
  }

  goToQuiz(event: Event | null, n: NotificationResponse): void {
    if (event) event.stopPropagation();
    const quizId = this.getQuizId(n);
    if (quizId) this.router.navigate(['/bo-cau-hoi/chi-tiet-bo-cau-hoi', quizId]).then(r => {
    });
  }
}
