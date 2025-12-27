import {Component, ElementRef, OnDestroy, OnInit, signal, ViewChild} from '@angular/core';
import {FormsModule} from '@angular/forms';
import {CommonModule} from '@angular/common';
import {Base} from '../../base/base';
import {ChatMessageResponse} from '../../../responses/chat/chat-message-response';
import {ResponseObject} from '../../../responses/response-object';
import {PageResponse} from '../../../responses/page-response';
import {SendMessageDto} from '../../../dtos/tinnhan/send-message-dto';
import {ChatFriendItemResponse} from '../../../responses/chat/chat-friend-item-response';
import {Subscription} from 'rxjs';
import {PickerComponent} from '@ctrl/ngx-emoji-mart';

@Component({
  selector: 'app-user-chat',
  standalone: true,
  imports: [CommonModule, FormsModule, PickerComponent],
  templateUrl: './user-chat.html',
  styleUrl: './user-chat.scss',
})
export class UserChat extends Base implements OnInit, OnDestroy {

  inbox_items: ChatFriendItemResponse[] = [];
  inbox_loading = false;

  messages: ChatMessageResponse[] = [];
  messages_loading = false;
  messages_page = 0;
  messages_limit = 30;
  messages_has_more = true;

  active_partner: ChatFriendItemResponse | null = null;
  new_message_noi_dung = '';
  sending = false;
  meName = ''; // Tên của user hiện tại để hiển thị Welcome
  showEmojiPicker = signal<boolean>(false);

  @ViewChild('messagesContainer') messages_container?: ElementRef<HTMLDivElement>;
  @ViewChild('chatInput') chatInput?: ElementRef<HTMLTextAreaElement>; // Reference tới textarea

  private route_partner_id: number | null = null;
  private wsSub?: Subscription;
  private current_user_id: number | null = null;

  ngOnInit(): void {
    const currentUser = this.userService.currentUser();
    this.current_user_id = currentUser?.id ?? null;
    this.meName = currentUser?.ho_ten || 'Bạn';

    if (this.current_user_id) {
      this.chatWsService.connect(this.current_user_id);
      this.wsSub = this.chatWsService.messages$.subscribe(msg => this.handleWsMessage(msg));
    }

    const param = this.route.snapshot.paramMap.get('partner_id');
    this.route_partner_id = param ? +param : null;

    this.load_friends();
  }

  ngOnDestroy(): void {
    this.wsSub?.unsubscribe();
    this.chatWsService.disconnect();
  }

  // --- 1. Load Inbox ---
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
          const found = this.inbox_items.find(it => it.partner_id === this.route_partner_id);
          if (found) this.select_partner(found);
        }
      },
      error: () => this.inbox_loading = false
    });
  }

  // --- 2. Select Partner ---
  select_partner(item: ChatFriendItemResponse): void {
    if (this.active_partner?.partner_id === item.partner_id) return;

    this.active_partner = item;
    this.messages = [];
    this.messages_page = 0;
    this.messages_has_more = true;
    item.unread_count = 0;

    this.load_messages(0);
    // Thay đổi URL mà không reload trang (Optional)
    // this.location.replaceState(`/chat/${item.partner_id}`);
  }

  load_messages(page: number): void {
    if (!this.active_partner || this.messages_loading) return;
    this.messages_loading = true;

    this.chatService.getConversation(this.active_partner.partner_id, page, this.messages_limit)
      .subscribe({
        next: (res: ResponseObject<PageResponse<ChatMessageResponse>>) => {
          const page_res = res.data;
          if (page_res) {
            this.messages_page = page_res.currentPage;
            this.messages_has_more = page_res.currentPage + 1 < page_res.totalPages;
            const items = (page_res.items || []).slice().reverse();

            if (page === 0) {
              this.messages = items;
              setTimeout(() => this.scroll_to_bottom(), 100);
            } else {
              this.messages = [...items, ...this.messages];
            }
          }
          this.messages_loading = false;
        },
        error: () => this.messages_loading = false
      });
  }

  load_more_messages(): void {
    if (this.messages_has_more && !this.messages_loading) {
      this.load_messages(this.messages_page + 1);
    }
  }

  // --- 3. Send Message ---
  send_message(): void {
    if (!this.active_partner) return;
    const content = this.new_message_noi_dung.trim();
    if (!content || this.sending) return;

    this.sending = true;
    const dto = new SendMessageDto();
    dto.receiver_id = this.active_partner.partner_id;
    dto.noi_dung = content;

    this.chatService.sendMessage(dto).subscribe({
      next: () => {
        this.new_message_noi_dung = '';
        this.sending = false;
        this.showEmojiPicker.set(false); // Đóng emoji khi gửi
        // Scroll bottom tạm thời
        if (this.chatInput?.nativeElement) {
          this.chatInput.nativeElement.style.height = 'auto';
        }
      },
      error: () => this.sending = false
    });
  }

  on_enter_send(event: Event): void {
    const kEvent = event as KeyboardEvent;
    if (!kEvent.shiftKey) {
      kEvent.preventDefault();
      this.send_message();
    }
  }

  // --- 4. WebSocket Handle ---
  private handleWsMessage(msg: ChatMessageResponse): void {
    if (!msg || !this.current_user_id) return;

    const meId = this.current_user_id;
    const partnerId = msg.gui_boi_id === meId ? msg.nhan_boi_id : msg.gui_boi_id;

    // Update Inbox List
    const idx = this.inbox_items.findIndex(it => it.partner_id === partnerId);
    if (idx >= 0) {
      const item = this.inbox_items[idx];
      const isCurrentOpened = this.active_partner && this.active_partner.partner_id === partnerId;

      const updated: ChatFriendItemResponse = {
        ...item,
        last_message: msg.noi_dung,
        last_time: msg.gui_luc,
        unread_count: isCurrentOpened ? 0 : (item.unread_count ?? 0) + 1,
      };

      this.inbox_items.splice(idx, 1);
      this.inbox_items.unshift(updated); // Move to top

      if (isCurrentOpened) this.active_partner = updated;
    }

    // Append Message if Chat Open
    if (this.active_partner && this.active_partner.partner_id === partnerId) {
      this.messages.push(msg);
      setTimeout(() => this.scroll_to_bottom(), 100);
    }
  }

  private scroll_to_bottom(): void {
    if (this.messages_container) {
      const el = this.messages_container.nativeElement;
      el.scrollTo({top: el.scrollHeight, behavior: 'smooth'});
    }
  }

  // Helper Avatar
  // --- Helper Avatar ---
// Thêm 'null' vào kiểu dữ liệu của tham số url
  getAvatar(url: string | null | undefined, name: string): string {
    if (url) {
      return `http://localhost:8088/api/v1/users/profile-images/${url}`;
    }
    // Fallback nếu url là null, undefined hoặc rỗng
    return `https://ui-avatars.com/api/?name=${name}&background=random&color=fff&size=128`;
  }

  // --- LOGIC EMOJI ---
  toggleEmojiPicker() {
    this.showEmojiPicker.update(v => !v);
  }

  addEmoji(event: any) {
    const emoji = event.emoji.native;
    this.new_message_noi_dung += emoji;
    // Sau khi thêm emoji thì gọi resize để ô nhập không bị che
    setTimeout(() => this.autoResize(), 0);
  }

  // --- LOGIC AUTO RESIZE TEXTAREA ---
  autoResize(): void {
    const textarea = this.chatInput?.nativeElement;
    if (textarea) {
      textarea.style.height = 'auto'; // Reset chiều cao để tính toán lại
      textarea.style.height = textarea.scrollHeight + 'px'; // Gán chiều cao theo nội dung
    }
  }
}
