import { CommonModule } from '@angular/common';
import { Component, ElementRef, HostListener, inject, OnInit } from '@angular/core';
import { RouterModule } from '@angular/router';
import { NotificationResponse } from '../../responses/notification/notification-response';
import { PageResponse } from '../../responses/page-response';
import { ResponseObject } from '../../responses/response-object';
import { FriendEventService } from '../../services/friend-event.service';
import { Base } from '../base/base';
import { ClickOutsideDirective } from './click-outside.directive';

@Component({
  selector: 'app-notification-bell',
  standalone: true,
  imports: [CommonModule, RouterModule, ClickOutsideDirective],
  templateUrl: './notification-bell.html',
  styleUrl: './notification-bell.scss',
})
export class NotificationBell extends Base implements OnInit {
  private friendEventService = inject(FriendEventService);

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

  constructor(private elementRef: ElementRef) {
    super();
  }

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

        // ⭐ Emit friend event để các component khác lắng nghe và refresh
        if (notif.loai === 'FRIEND_REQUEST') {
          this.friendEventService.handleNotification(notif);
        }

        const msg =
          notif.noi_dung ||
          (notif.loai === 'FRIEND_REQUEST'
            ? `${notif.nguoi_gui_ten} đã gửi cho bạn lời mời kết bạn`
            : 'Bạn có thông báo mới');

        const type = notif.loai === 'FRIEND_REQUEST' ? ('info' as const) : ('success' as const);

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

  @HostListener('document:click', ['$event'])
  onDocumentClick(event: MouseEvent): void {
    if (!this.elementRef.nativeElement.contains(event.target)) {
      this.show_dropdown = false;
    }
  }

  loadUnreadCount(): void {
    this.notificationService.getUnreadCount().subscribe({
      next: (res: ResponseObject<any>) => {
        this.unread_count = res.data ?? 0;
        console.log('Loaded unread notification count:', this.unread_count);
      },
      error: () => {
        this.unread_count = 0;
      },
    });
  }

  toggleDropdown(event: Event): void {
    event.stopPropagation(); // 🛑 Dừng không cho lan lên document
    console.log('Toggling notification dropdown');
    this.show_dropdown = !this.show_dropdown;
    console.log('Dropdown is now', this.show_dropdown ? 'shown' : 'hidden');
    if (this.show_dropdown && this.notifications.length === 0) {
      this.loadPage(0);
      console.log('Loading first page of notifications');
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
      },
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
        error: () => {},
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
        this.notifications.forEach((n) => (n.da_doc = true));
        this.unread_count = 0;
      },
      error: () => {},
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

    // Thông báo liên quan đến bộ câu hỏi (loại chi tiết nằm trong metadata.type)
    if ((meta?.type === 'QUIZ_APPROVED' || meta?.type === 'QUIZ_UNLOCKED') && meta?.bo_cau_hoi_id) {
      return ['/bo-cau-hoi/chi-tiet-bo-cau-hoi', meta.bo_cau_hoi_id];
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
        next: () => {},
        error: () => {},
      });
    }

    this.closeBattleInviteToast();
    this.router.navigate(['/tran-dau/phong', tranDauId]).then();
  }

  dismissBattleInvite(): void {
    const notif = this.battle_invite_toast;
    if (notif && !notif.da_doc) {
      this.notificationService.markAsRead(notif.thong_bao_id).subscribe({
        next: () => {},
        error: () => {},
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

  // ================== Helper UI cho template ==================
  getDisplayType(n: NotificationResponse): string {
    const t = this.getTypeFromMetadata(n);
    switch (t) {
      case 'FRIEND_REQUEST':
        return 'Lời mời kết bạn';
      case 'BATTLE_INVITE':
        return 'Lời mời trận đấu';
      case 'QUIZ_APPROVED':
        return 'Bộ câu hỏi được duyệt';
      case 'QUIZ_UNLOCKED':
        return 'Bộ câu hỏi được mở khóa';
      case 'SYSTEM':
      default:
        return 'Hệ thống';
    }
  }

  isQuizNotification(n: NotificationResponse): boolean {
    const t = this.getTypeFromMetadata(n);
    return t === 'QUIZ_APPROVED' || t === 'QUIZ_UNLOCKED';
  }

  getIconClass(notif: any): string {
    const type = this.getTypeFromMetadata(notif);
    switch (type) {
      case 'FRIEND_REQUEST':
        return 'friend';
      case 'BATTLE_INVITE':
        return 'battle';
      case 'QUIZ_APPROVED':
      case 'QUIZ_UNLOCKED':
        return 'gold'; // Hoặc tạo class 'quiz' riêng
      default:
        return 'system';
    }
  }

  // Lấy Icon FontAwesome
  getIcon(notif: any): string {
    const type = this.getTypeFromMetadata(notif);
    switch (type) {
      case 'FRIEND_REQUEST':
        return 'fas fa-user-plus';
      case 'BATTLE_INVITE':
        return 'fas fa-swords'; // Cần FontAwesome Pro hoặc dùng fa-gamepad
      case 'QUIZ_APPROVED':
        return 'fas fa-check-circle';
      case 'QUIZ_UNLOCKED':
        return 'fas fa-unlock-alt';
      default:
        return 'fas fa-bell';
    }
  }

  // Lấy thông tin loại từ metadata (nếu có)
  private getTypeFromMetadata(n: any): string {
    if (n.metadata) {
      try {
        const meta = JSON.parse(n.metadata);
        if (typeof meta?.type === 'string') return meta.type;
      } catch {}
    }
    return n.loai;
  }

  // Lấy số vàng thưởng (nếu có)
  getGoldReward(n: any): number | null {
    if (!n.metadata) return null;
    try {
      const meta = JSON.parse(n.metadata);
      return typeof meta?.gold_reward === 'number' ? meta.gold_reward : null;
    } catch {
      return null;
    }
  }
}
