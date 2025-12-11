import {Component, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import {NotificationResponse} from '../../../responses/notification/notification-response';
import {ResponseObject} from '../../../responses/response-object';
import {PageResponse} from '../../../responses/page-response';
import {Base} from '../../base/base';

@Component({
  selector: 'app-user-notification-page',
  standalone: true,
  imports: [CommonModule],
  templateUrl: './user-notification-page.html',
  styleUrl: './user-notification-page.scss'
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
    this.notificationService.getMy(page, this.limit).subscribe({
      next: (res: ResponseObject<PageResponse<NotificationResponse>>) => {
        const data = res.data;
        console.log('Notification page data', data);
        if (data) {
          let list = data.items || (data as any).content || [];
          if (this.only_unread) {
            list = list.filter((n: NotificationResponse) => !n.da_doc);
          }
          this.notifications = list;
          this.page = data.currentPage;
          this.total_pages = data.totalPages;
        }
        this.loading = false;
      },
      error: () => {
        this.loading = false;
      }
    });
  }

  toggleFilterUnread(): void {
    this.only_unread = !this.only_unread;
    this.loadPage(0);
  }

  prevPage(): void {
    if (this.page > 0) {
      this.loadPage(this.page - 1);
    }
  }

  nextPage(): void {
    if (this.page + 1 < this.total_pages) {
      this.loadPage(this.page + 1);
    }
  }
}
