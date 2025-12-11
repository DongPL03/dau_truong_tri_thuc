// src/app/components/chat/user-chat/user-chat.ts
import {Component, ElementRef, inject, OnDestroy, OnInit, ViewChild} from '@angular/core';
import {FormsModule} from '@angular/forms';
import {CommonModule} from '@angular/common';
import {Base} from '../../base/base';
import {ChatMessageResponse} from '../../../responses/chat/chat-message-response';
import {ResponseObject} from '../../../responses/response-object';
import {PageResponse} from '../../../responses/page-response';
import {SendMessageDto} from '../../../dtos/tinnhan/send-message-dto';
import {ChatFriendItemResponse} from '../../../responses/chat/chat-friend-item-response';
import {ChatWsService} from '../../../services/chat-ws.service';
import {Subscription} from 'rxjs';

@Component({
  selector: 'app-user-chat',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './user-chat.html',
  styleUrl: './user-chat.scss',
})
export class UserChat extends Base implements OnInit, OnDestroy {

  // 🧾 Danh sách bạn bè (inbox)
  inbox_items: ChatFriendItemResponse[] = [];
  inbox_loading = false;

  // 💬 Messages
  messages: ChatMessageResponse[] = [];
  messages_loading = false;
  messages_page = 0;
  messages_limit = 30;
  messages_has_more = true;

  active_partner: ChatFriendItemResponse | null = null;
  new_message_noi_dung = '';
  sending = false;

  default_avatar = 'assets/images/default-profile-image.jpeg';

  @ViewChild('messagesContainer')
  messages_container?: ElementRef<HTMLDivElement>;

  private route_partner_id: number | null = null;

  private wsSub?: Subscription;
  private current_user_id: number | null = null;

  ngOnInit(): void {
    // Lấy user hiện tại
    const currentUser = this.userService.currentUser();
    this.current_user_id = currentUser?.id ?? null;

    // Kết nối WS chat
    if (this.current_user_id) {
      this.chatWsService.connect(this.current_user_id);
      this.wsSub = this.chatWsService.messages$
        .subscribe(msg => this.handleWsMessage(msg));
    }

    // Partner id từ route (nếu có /chat/:partner_id)
    const param = this.route.snapshot.paramMap.get('partner_id');
    this.route_partner_id = param ? +param : null;

    // Load danh sách bạn bè
    this.load_friends();
  }

  ngOnDestroy(): void {
    this.wsSub?.unsubscribe();
    this.chatWsService.disconnect();
  }

  // =============================
  // 1. Load friends
  // =============================
  load_friends(): void {
    if (this.inbox_loading) return;
    this.inbox_loading = true;

    this.friendService.getFriends().subscribe({
      next: res => {
        const friends = res.data || [];
        this.inbox_items = friends.map((f: any) => ({
          partner_id: f.user_id ?? f.userId,
          partner_name: f.ho_ten ?? f.hoTen,
          partner_avatar_url: f.avatar_url ?? f.avatarUrl,
          partner_status: f.trang_thai ?? 'OFFLINE',
          last_message: null,
          last_time: null,
          unread_count: 0,
        }));
        this.inbox_loading = false;

        if (this.route_partner_id && !this.active_partner) {
          const found = this.inbox_items.find(
            it => it.partner_id === this.route_partner_id
          );
          if (found) {
            this.select_partner(found);
          }
        }
      },
      error: () => {
        this.inbox_loading = false;
      }
    });
  }

  // =============================
  // 2. Chọn partner & load messages
  // =============================
  select_partner(item: ChatFriendItemResponse): void {
    if (this.active_partner?.partner_id === item.partner_id) {
      return;
    }

    this.active_partner = item;
    this.messages = [];
    this.messages_page = 0;
    this.messages_has_more = true;

    // reset unread
    item.unread_count = 0;

    this.load_messages(0);

    this.router.navigate(['/chat', item.partner_id]).then();
  }

  load_messages(page: number): void {
    if (!this.active_partner || this.messages_loading) {
      return;
    }

    this.messages_loading = true;

    this.chatService.getConversation(
      this.active_partner.partner_id,
      page,
      this.messages_limit
    ).subscribe({
      next: (res: ResponseObject<PageResponse<ChatMessageResponse>>) => {
        const page_res = res.data;
        if (!page_res) {
          this.messages_loading = false;
          return;
        }

        this.messages_page = page_res.currentPage;
        this.messages_has_more = page_res.currentPage + 1 < page_res.totalPages;

        const items = (page_res.items || []).slice().reverse();

        if (page === 0) {
          this.messages = items;
          setTimeout(() => this.scroll_to_bottom(), 0);
        } else {
          this.messages = [...items, ...this.messages];
        }

        this.messages_loading = false;
      },
      error: () => {
        this.messages_loading = false;
      }
    });
  }

  load_more_messages(): void {
    if (this.messages_has_more && !this.messages_loading) {
      this.load_messages(this.messages_page + 1);
    }
  }

  // =============================
  // 3. Gửi tin nhắn
  // =============================
  send_message(): void {
    if (!this.active_partner) {
      return;
    }
    const content = this.new_message_noi_dung.trim();
    if (!content || this.sending) {
      return;
    }

    this.sending = true;
    const dto = new SendMessageDto();
    dto.receiver_id = this.active_partner.partner_id;
    dto.noi_dung = content;

    this.chatService.sendMessage(dto).subscribe({
      next: () => {
        // Không tự thêm vào this.messages nữa,
        // đợi WS đẩy về cho cả 2 phía
        this.new_message_noi_dung = '';
        this.sending = false;
      },
      error: () => {
        this.sending = false;
      }
    });
  }

  on_enter_send(event: Event): void {
    const kEvent = event as KeyboardEvent;
    if (!kEvent.shiftKey) {
      kEvent.preventDefault();
      this.send_message();
    }
  }

  // =============================
  // 4. Handle WS message
  // =============================
  private handleWsMessage(msg: ChatMessageResponse): void {
    if (!msg || !this.current_user_id) {
      return;
    }

    const meId = this.current_user_id;
    const partnerId = msg.gui_boi_id === meId ? msg.nhan_boi_id : msg.gui_boi_id;

    // 4.1. Cập nhật danh sách bạn bè (last_message, last_time, unread_count)
    const idx = this.inbox_items.findIndex(it => it.partner_id === partnerId);
    if (idx >= 0) {
      const item = this.inbox_items[idx];
      const isCurrentOpened = this.active_partner && this.active_partner.partner_id === partnerId;
      const unread = isCurrentOpened ? item.unread_count ?? 0 : (item.unread_count ?? 0) + 1;

      const updated: ChatFriendItemResponse = {
        ...item,
        last_message: msg.noi_dung,
        last_time: msg.gui_luc,
        unread_count: unread,
      };

      // Đưa cuộc hội thoại này lên đầu list
      const newList = this.inbox_items.slice();
      newList.splice(idx, 1);
      this.inbox_items = [updated, ...newList];

      if (isCurrentOpened) {
        this.active_partner = updated;
      }
    }

    // 4.2. Nếu đang mở đúng partner → append message
    if (this.active_partner && this.active_partner.partner_id === partnerId) {
      this.messages = [...this.messages, msg];
      setTimeout(() => this.scroll_to_bottom(), 0);
    }
  }

  private scroll_to_bottom(): void {
    if (!this.messages_container) {
      return;
    }
    const el = this.messages_container.nativeElement;
    el.scrollTop = el.scrollHeight;
  }
}
