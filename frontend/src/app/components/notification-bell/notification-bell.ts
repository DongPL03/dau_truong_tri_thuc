import {Component, HostListener, OnInit} from '@angular/core';
import {CommonModule} from '@angular/common';
import {RouterModule} from '@angular/router';
import {Base} from '../base/base';
import {NotificationResponse} from '../../responses/notification/notification-response';
import {ResponseObject} from '../../responses/response-object';
import {PageResponse} from '../../responses/page-response';

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

  // Toast lời mời trận đấu
  battle_invite_toast: NotificationResponse | null = null;
  battle_invite_tran_dau_id: number | null = null;
  battle_invite_ma_phong: string | null = null;
  private battle_invite_timer: any = null;


  ngOnInit(): void {
    this.loadUnreadCount();
    const currentUser = this.userService.currentUser();
    if (currentUser?.id) {
      // Kết nối WS và listen
      this.notificationWsService.connect(currentUser.id);
      this.notificationWsService.notifications$.subscribe((notif) => {
        // Thêm vào đầu list
        this.notifications = [notif, ...this.notifications];

        // Tăng badge nếu là chưa đọc
        if (!notif.da_doc) {
          this.unread_count++;
        }

        // ⭐ Tách xử lý theo loại
        if (notif.loai === 'BATTLE_INVITE') {
          // ---- 1. Chỉ hiển thị khung mời phòng đấu, KHÔNG gọi toastService.show ----
          let meta: any = null;
          try {
            meta = notif.metadata ? JSON.parse(notif.metadata) : null;
          } catch {
            meta = null;
          }

          this.battle_invite_tran_dau_id = meta?.tran_dau_id ?? null;
          this.battle_invite_ma_phong = meta?.ma_phong ?? null;
          this.battle_invite_toast = notif;

          // auto hide sau 15s
          if (this.battle_invite_timer) {
            clearTimeout(this.battle_invite_timer);
          }
          this.battle_invite_timer = setTimeout(() => {
            this.battle_invite_toast = null;
            this.battle_invite_timer = null;
          }, 15000);

          return; // ⛔ stop ở đây, không chạy xuống toast thường nữa
        }

        // ---- 2. Các loại notif khác (FRIEND_REQUEST, SYSTEM...) dùng toast thường ----
        const msg =
          notif.noi_dung ||
          (notif.loai === 'FRIEND_REQUEST'
            ? `${notif.nguoi_gui_ten} đã gửi cho bạn lời mời kết bạn`
            : 'Bạn có thông báo mới');

        const type =
          notif.loai === 'FRIEND_REQUEST'
            ? ('info' as const)
            : ('success' as const);

        const route = this.build_toast_route_from_notif(notif);

        // duration_ms = 6000 cho có thời gian đọc & bấm
        this.toastService.show(msg, type, 6000, route);
      });
    }
  }


  ngOnDestroy(): void {
    // nếu NotificationWsService có hàm disconnect thì gọi thêm:
    this.notificationWsService.ngOnDestroy();
  }

  /** Đóng dropdown khi click ra ngoài */
  @HostListener('document:click')
  onDocumentClick(): void {
    this.show_dropdown = false;
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
    if (this.loading) {
      return;
    }
    this.loading = true;

    this.notificationService.getMy(page, this.limit).subscribe({
      next: (res: ResponseObject<any>) => {
        const pageRes = res.data as PageResponse<NotificationResponse>;
        if (!pageRes) {
          this.loading = false;
          return;
        }
        this.page = pageRes.currentPage;
        this.has_more = pageRes.currentPage + 1 < pageRes.totalPages;
        const items = pageRes.items || [];
        if (page === 0) {
          this.notifications = items;
        } else {
          this.notifications = [...this.notifications, ...items];
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
        error: () => {
        }
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
      error: () => {
      }
    });
  }

  private build_toast_route_from_notif(notif: NotificationResponse): any[] | undefined {
    let meta: any = null;
    try {
      meta = notif.metadata ? JSON.parse(notif.metadata) : null;
    } catch {
      meta = null;
    }

    if (notif.loai === 'FRIEND_REQUEST') {
      // danh sách lời mời kết bạn
      return ['/ban-be'];
    }

    if (notif.loai === 'BATTLE_INVITE' && meta?.tran_dau_id) {
      // ví dụ: xem chi tiết / tham gia trận đấu
      return ['/tran-dau', meta.tran_dau_id];
    }

    // các loại khác: chưa cần navigate
    return undefined;
  }

  acceptBattleInvite(): void {
    const notif = this.battle_invite_toast;
    const tranDauId = this.battle_invite_tran_dau_id;
    console.log('Accept battle invite to match id:', tranDauId);
    if (!notif || !tranDauId) {
      return;
    }

    // đánh dấu đã đọc (best effort, lỗi cũng không sao)
    if (!notif.da_doc) {
      this.notificationService.markAsRead(notif.thong_bao_id).subscribe({
        next: () => {
        },
        error: () => {
        }
      });
    }

    this.closeBattleInviteToast();
    this.router.navigate(['/tran-dau/phong', tranDauId]).then();
  }

  dismissBattleInvite(): void {
    const notif = this.battle_invite_toast;
    if (notif && !notif.da_doc) {
      this.notificationService.markAsRead(notif.thong_bao_id).subscribe({
        next: () => {
        },
        error: () => {
        }
      });
    }
    this.closeBattleInviteToast();
  }

  private closeBattleInviteToast(): void {
    this.battle_invite_toast = null;
    if (this.battle_invite_timer) {
      clearTimeout(this.battle_invite_timer);
      this.battle_invite_timer = null;
    }
  }

}
