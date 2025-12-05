
import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import {Router, RouterModule} from '@angular/router';
import {Base} from '../base/base';
import {NotificationResponse} from '../../responses/notification/notification-response';
import {ResponseObject} from '../../responses/response-object';
@Component({
  selector: 'app-notification-bell',
  standalone: true,
  imports: [CommonModule, RouterModule],
  templateUrl: './notification-bell.html',
  styleUrl: './notification-bell.scss'
})


export class NotificationBell extends Base implements OnInit {


  unread_count = 0;
  notifications: NotificationResponse[] = [];
  loading = false;
  show_dropdown = false;

  page = 0;
  limit = 10;
  has_more = true;


  ngOnInit(): void {
    this.loadUnreadCount();
  }

  loadUnreadCount(): void {
    this.notificationService.getUnreadCount().subscribe({
      next: (res: ResponseObject<any>) => {
        this.unread_count = res.data ?? 0;
      },
      error: () => {
        this.unread_count = 0;
      }
    });
  }

  toggleDropdown(): void {
    this.show_dropdown = !this.show_dropdown;
    if (this.show_dropdown && this.notifications.length === 0) {
      this.loadPage(0);
    }
  }

  loadPage(page: number): void {
    if (this.loading) { return; }
    this.loading = true;

    this.notificationService.getMy(page, this.limit).subscribe({
      next: (res: ResponseObject<any>) => {
        const pageRes = res.data;
        if (!pageRes) {
          this.loading = false;
          return;
        }
        this.page = pageRes.page_number;
        this.has_more = !pageRes.last;
        if (page === 0) {
          this.notifications = pageRes.items || pageRes.content || [];
        } else {
          this.notifications = [
            ...this.notifications,
            ...(pageRes.items || pageRes.content || [])
          ];
        }
        this.loading = false;
      },
      error: () => {
        this.loading = false;
      }
    });
  }

  loadMore(): void {
    if (this.has_more) {
      this.loadPage(this.page + 1);
    }
  }

  onClickItem(notif: NotificationResponse): void {
    if (!notif.da_doc) {
      this.notificationService.markAsRead(notif.thong_bao_id).subscribe({
        next: () => {
          notif.da_doc = true;
          if (this.unread_count > 0) {
            this.unread_count--;
          }
        },
        error: () => {}
      });
    }

    // Điều hướng dựa trên loại + metadata
    let meta: any = null;
    try {
      meta = notif.metadata ? JSON.parse(notif.metadata) : null;
    } catch {
      meta = null;
    }

    if (notif.loai === 'FRIEND_REQUEST') {
      this.router.navigate(['/ban-be']).then();
    } else if (notif.loai === 'BATTLE_INVITE' && meta?.tran_dau_id) {
      this.router.navigate(['/tran-dau', meta.tran_dau_id]).then();
    } else {
      // System / default
      // Có thể sau này làm trang /thong-bao chi tiết
    }
  }

  markAllRead(): void {
    this.notificationService.markAllAsRead().subscribe({
      next: () => {
        this.notifications.forEach(n => n.da_doc = true);
        this.unread_count = 0;
      },
      error: () => {}
    });
  }
}
