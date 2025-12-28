// import { CommonModule } from '@angular/common';
// import { Component, OnDestroy, OnInit } from '@angular/core';
// import { FormsModule } from '@angular/forms';
// import { ActivatedRoute, Router, RouterModule } from '@angular/router';
// import { Subject, takeUntil } from 'rxjs';
// import {
//   ChatWebSocketMessage,
//   LoaiPhongChat,
//   LoaiTinNhan,
//   PhongChat,
//   TinNhan,
//   TypingUser,
// } from '../../../models/chat';
// import { ChatService } from '../../../services/chat.service';
// import { TokenService } from '../../../services/token.service';
//
// @Component({
//   selector: 'app-chat-main',
//   standalone: true,
//   imports: [CommonModule, FormsModule, RouterModule],
//   templateUrl: './chat-main.html',
//   styleUrl: './chat-main.scss',
// })
// export class ChatMainComponent implements OnInit, OnDestroy {
//   // Rooms
//   rooms: PhongChat[] = [];
//   pinnedRooms: PhongChat[] = [];
//   activeRoom: PhongChat | null = null;
//   roomsLoading = false;
//   roomsPage = 0;
//   hasMoreRooms = true;
//
//   // Messages
//   messages: TinNhan[] = [];
//   messagesLoading = false;
//   hasMoreMessages = true;
//
//   // Input
//   newMessage = '';
//   replyingTo: TinNhan | null = null;
//   isSending = false;
//
//   // Search
//   searchQuery = '';
//   isSearching = false;
//   searchResults: PhongChat[] = [];
//
//   // Typing
//   typingUsers: TypingUser[] = [];
//
//   // Create group modal
//   showCreateGroupModal = false;
//   newGroupName = '';
//   newGroupMembers: number[] = [];
//
//   // Room info panel
//   showRoomInfo = false;
//
//   // Current user
//   currentUserId: number | null = null;
//
//   private destroy$ = new Subject<void>();
//
//   LoaiPhongChat = LoaiPhongChat;
//   LoaiTinNhan = LoaiTinNhan;
//
//   constructor(
//     private chatService: ChatService,
//     private tokenService: TokenService,
//     private route: ActivatedRoute,
//     private router: Router
//   ) {}
//
//   ngOnInit(): void {
//     this.currentUserId = this.tokenService.getUserId();
//
//     // Connect WebSocket
//     this.chatService.connect();
//
//     // Subscribe to WebSocket messages
//     this.chatService.messages$.pipe(takeUntil(this.destroy$)).subscribe((msg) => {
//       this.handleWebSocketMessage(msg);
//     });
//
//     // Subscribe to typing indicators
//     this.chatService.typingUsers$.pipe(takeUntil(this.destroy$)).subscribe((typingMap) => {
//       if (this.activeRoom) {
//         this.typingUsers = typingMap.get(this.activeRoom.id) || [];
//       }
//     });
//
//     // Load initial data
//     this.loadRooms();
//     this.loadPinnedRooms();
//
//     // Check for room ID in route
//     this.route.params.pipe(takeUntil(this.destroy$)).subscribe((params) => {
//       if (params['roomId']) {
//         this.openRoom(+params['roomId']);
//       }
//     });
//
//     // Check for user ID to start chat (from friend list, etc.)
//     this.route.queryParams.pipe(takeUntil(this.destroy$)).subscribe((params) => {
//       if (params['userId']) {
//         this.startChatWithUser(+params['userId']);
//       }
//     });
//   }
//
//   ngOnDestroy(): void {
//     this.destroy$.next();
//     this.destroy$.complete();
//     this.chatService.disconnect();
//   }
//
//   // ============== LOAD DATA ==============
//
//   loadRooms(): void {
//     if (this.roomsLoading) return;
//
//     this.roomsLoading = true;
//     this.chatService
//       .getRooms(this.roomsPage, 20)
//       .pipe(takeUntil(this.destroy$))
//       .subscribe({
//         next: (res) => {
//           if (res.status === 'OK' && res.data) {
//             if (this.roomsPage === 0) {
//               this.rooms = res.data.items;
//             } else {
//               this.rooms = [...this.rooms, ...res.data.items];
//             }
//             this.hasMoreRooms = res.data.items.length === 20;
//           }
//           this.roomsLoading = false;
//         },
//         error: () => {
//           this.roomsLoading = false;
//         },
//       });
//   }
//
//   loadPinnedRooms(): void {
//     this.chatService
//       .getPinnedRooms()
//       .pipe(takeUntil(this.destroy$))
//       .subscribe({
//         next: (res) => {
//           if (res.status === 'OK' && res.data) {
//             this.pinnedRooms = res.data;
//           }
//         },
//       });
//   }
//
//   loadMoreRooms(): void {
//     if (!this.hasMoreRooms || this.roomsLoading) return;
//     this.roomsPage++;
//     this.loadRooms();
//   }
//
//   loadMessages(roomId: number): void {
//     if (this.messagesLoading) return;
//
//     this.messagesLoading = true;
//     this.chatService
//       .getMessages(roomId, 0, 30)
//       .pipe(takeUntil(this.destroy$))
//       .subscribe({
//         next: (res) => {
//           if (res.status === 'OK' && res.data) {
//             this.messages = res.data.items.reverse(); // Reverse để hiển thị từ cũ đến mới
//             this.hasMoreMessages = res.data.items.length === 30;
//             this.scrollToBottom();
//           }
//           this.messagesLoading = false;
//         },
//         error: () => {
//           this.messagesLoading = false;
//         },
//       });
//   }
//
//   loadMoreMessages(): void {
//     if (
//       !this.hasMoreMessages ||
//       this.messagesLoading ||
//       !this.activeRoom ||
//       this.messages.length === 0
//     )
//       return;
//
//     const firstMessage = this.messages[0];
//     this.messagesLoading = true;
//
//     this.chatService
//       .getMessagesBefore(this.activeRoom.id, firstMessage.id, 30)
//       .pipe(takeUntil(this.destroy$))
//       .subscribe({
//         next: (res) => {
//           if (res.status === 'OK' && res.data) {
//             this.messages = [...res.data.items.reverse(), ...this.messages];
//             this.hasMoreMessages = res.data.items.length === 30;
//           }
//           this.messagesLoading = false;
//         },
//         error: () => {
//           this.messagesLoading = false;
//         },
//       });
//   }
//
//   // ============== ROOM ACTIONS ==============
//
//   openRoom(roomId: number): void {
//     if (this.activeRoom?.id === roomId) return;
//
//     // Unsubscribe from previous room
//     if (this.activeRoom) {
//       this.chatService.unsubscribeFromRoom(this.activeRoom.id);
//     }
//
//     // Find room in list
//     let room =
//       this.rooms.find((r) => r.id === roomId) || this.pinnedRooms.find((r) => r.id === roomId);
//
//     if (room) {
//       this.activeRoom = room;
//       this.chatService.subscribeToRoom(roomId);
//       this.loadMessages(roomId);
//       this.markAsRead(roomId);
//     } else {
//       // Load room detail
//       this.chatService
//         .getRoomDetail(roomId)
//         .pipe(takeUntil(this.destroy$))
//         .subscribe({
//           next: (res) => {
//             if (res.status === 'OK' && res.data) {
//               this.activeRoom = res.data;
//               this.chatService.subscribeToRoom(roomId);
//               this.loadMessages(roomId);
//               this.markAsRead(roomId);
//             }
//           },
//         });
//     }
//
//     this.messages = [];
//     this.replyingTo = null;
//     this.typingUsers = [];
//   }
//
//   startChatWithUser(userId: number): void {
//     this.chatService
//       .getOrCreatePrivateChat(userId)
//       .pipe(takeUntil(this.destroy$))
//       .subscribe({
//         next: (res) => {
//           if (res.status === 'OK' && res.data) {
//             this.router.navigate(['/chat', res.data.id]);
//           }
//         },
//       });
//   }
//
//   markAsRead(roomId: number): void {
//     this.chatService.markAsRead(roomId).pipe(takeUntil(this.destroy$)).subscribe();
//
//     // Update unread count locally
//     const room = this.rooms.find((r) => r.id === roomId);
//     if (room) {
//       room.soTinNhanChuaDoc = 0;
//     }
//   }
//
//   togglePin(room: PhongChat, event: Event): void {
//     event.stopPropagation();
//
//     this.chatService
//       .togglePinRoom(room.id)
//       .pipe(takeUntil(this.destroy$))
//       .subscribe({
//         next: (res) => {
//           if (res.status === 'OK' && res.data) {
//             room.daGhim = res.data.daGhim;
//             this.loadPinnedRooms();
//           }
//         },
//       });
//   }
//
//   toggleMute(room: PhongChat, event: Event): void {
//     event.stopPropagation();
//
//     this.chatService
//       .toggleMuteRoom(room.id)
//       .pipe(takeUntil(this.destroy$))
//       .subscribe({
//         next: (res) => {
//           if (res.status === 'OK' && res.data) {
//             room.daTatThongBao = res.data.daTatThongBao;
//           }
//         },
//       });
//   }
//
//   leaveRoom(): void {
//     if (!this.activeRoom || this.activeRoom.loai === LoaiPhongChat.DON) return;
//
//     if (confirm('Bạn có chắc muốn rời khỏi nhóm này?')) {
//       this.chatService
//         .leaveRoom(this.activeRoom.id)
//         .pipe(takeUntil(this.destroy$))
//         .subscribe({
//           next: () => {
//             this.rooms = this.rooms.filter((r) => r.id !== this.activeRoom?.id);
//             this.activeRoom = null;
//             this.messages = [];
//           },
//         });
//     }
//   }
//
//   // ============== MESSAGE ACTIONS ==============
//
//   sendMessage(): void {
//     if (!this.activeRoom || !this.newMessage.trim() || this.isSending) return;
//
//     this.isSending = true;
//     const dto = {
//       phongChatId: this.activeRoom.id,
//       noiDung: this.newMessage.trim(),
//       loai: LoaiTinNhan.VAN_BAN,
//       traLoiChoId: this.replyingTo?.id,
//     };
//
//     this.chatService
//       .sendMessage(dto)
//       .pipe(takeUntil(this.destroy$))
//       .subscribe({
//         next: (res) => {
//           if (res.status === 'CREATED' && res.data) {
//             // Message will be added via WebSocket
//             this.newMessage = '';
//             this.replyingTo = null;
//           }
//           this.isSending = false;
//         },
//         error: () => {
//           this.isSending = false;
//         },
//       });
//
//     this.chatService.sendStopTyping(this.activeRoom.id);
//   }
//
//   onInputKeydown(event: KeyboardEvent): void {
//     if (event.key === 'Enter' && !event.shiftKey) {
//       event.preventDefault();
//       this.sendMessage();
//     } else if (this.activeRoom) {
//       this.chatService.sendTyping(this.activeRoom.id);
//     }
//   }
//
//   replyTo(message: TinNhan): void {
//     this.replyingTo = message;
//   }
//
//   cancelReply(): void {
//     this.replyingTo = null;
//   }
//
//   deleteMessage(message: TinNhan): void {
//     if (confirm('Xóa tin nhắn này?')) {
//       this.chatService
//         .deleteMessage(message.id)
//         .pipe(takeUntil(this.destroy$))
//         .subscribe({
//           next: () => {
//             this.messages = this.messages.filter((m) => m.id !== message.id);
//           },
//         });
//     }
//   }
//
//   // ============== SEARCH ==============
//
//   onSearch(): void {
//     if (!this.searchQuery.trim()) {
//       this.searchResults = [];
//       this.isSearching = false;
//       return;
//     }
//
//     this.isSearching = true;
//     this.chatService
//       .searchRooms(this.searchQuery, 0, 20)
//       .pipe(takeUntil(this.destroy$))
//       .subscribe({
//         next: (res) => {
//           if (res.status === 'OK' && res.data) {
//             this.searchResults = res.data.items;
//           }
//           this.isSearching = false;
//         },
//         error: () => {
//           this.isSearching = false;
//         },
//       });
//   }
//
//   clearSearch(): void {
//     this.searchQuery = '';
//     this.searchResults = [];
//     this.isSearching = false;
//   }
//
//   // ============== WEBSOCKET HANDLERS ==============
//
//   private handleWebSocketMessage(msg: ChatWebSocketMessage): void {
//     switch (msg.type) {
//       case 'NEW_MESSAGE':
//         this.handleNewMessage(msg.data as TinNhan);
//         break;
//       case 'USER_READ':
//         // Handle read receipt
//         break;
//     }
//   }
//
//   private handleNewMessage(message: TinNhan): void {
//     // Add to current room messages if it's the active room
//     if (this.activeRoom && message.phongChatId === this.activeRoom.id) {
//       this.messages.push(message);
//       this.scrollToBottom();
//
//       // Mark as read if window is focused
//       if (document.hasFocus()) {
//         this.markAsRead(this.activeRoom.id);
//       }
//     }
//
//     // Update room list
//     const room = this.rooms.find((r) => r.id === message.phongChatId);
//     if (room) {
//       room.tinNhanCuoi = message.noiDung || this.getMessagePreview(message);
//       room.thoiGianTinNhanCuoi = message.guiLuc;
//
//       if (!this.activeRoom || this.activeRoom.id !== room.id) {
//         room.soTinNhanChuaDoc++;
//       }
//
//       // Move to top
//       this.rooms = [room, ...this.rooms.filter((r) => r.id !== room.id)];
//     }
//   }
//
//   // ============== HELPERS ==============
//
//   getTypingUsersNames(): string {
//     return this.typingUsers.map((u) => u.userName).join(', ');
//   }
//
//   getRoomDisplayName(room: PhongChat): string {
//     if (room.loai === LoaiPhongChat.NHOM) {
//       return room.ten || 'Nhóm chat';
//     }
//     return room.nguoiChat?.ten || 'Chat';
//   }
//
//   getRoomAvatar(room: PhongChat): string {
//     if (room.loai === LoaiPhongChat.NHOM) {
//       return room.anhNhom || 'assets/images/default-group.png';
//     }
//     return room.nguoiChat?.anhDaiDien || 'assets/images/default-avatar.png';
//   }
//
//   getMessagePreview(message: TinNhan): string {
//     switch (message.loai) {
//       case LoaiTinNhan.HINH_ANH:
//         return '📷 Hình ảnh';
//       case LoaiTinNhan.TAP_TIN:
//         return '📎 ' + (message.tenFile || 'Tập tin');
//       case LoaiTinNhan.AM_THANH:
//         return '🎵 Tin nhắn thoại';
//       case LoaiTinNhan.HE_THONG:
//         return message.noiDung || 'Thông báo';
//       default:
//         return message.noiDung || '';
//     }
//   }
//
//   formatTime(dateString: string): string {
//     const date = new Date(dateString);
//     const now = new Date();
//     const diff = now.getTime() - date.getTime();
//     const days = Math.floor(diff / (1000 * 60 * 60 * 24));
//
//     if (days === 0) {
//       return date.toLocaleTimeString('vi-VN', { hour: '2-digit', minute: '2-digit' });
//     } else if (days === 1) {
//       return 'Hôm qua';
//     } else if (days < 7) {
//       return date.toLocaleDateString('vi-VN', { weekday: 'short' });
//     } else {
//       return date.toLocaleDateString('vi-VN', { day: '2-digit', month: '2-digit' });
//     }
//   }
//
//   formatMessageTime(dateString: string): string {
//     return new Date(dateString).toLocaleTimeString('vi-VN', {
//       hour: '2-digit',
//       minute: '2-digit',
//     });
//   }
//
//   isMyMessage(message: TinNhan): boolean {
//     return message.laToi || message.nguoiGui?.id === this.currentUserId;
//   }
//
//   shouldShowAvatar(index: number): boolean {
//     if (index === this.messages.length - 1) return true;
//     const currentMsg = this.messages[index];
//     const nextMsg = this.messages[index + 1];
//     return currentMsg.nguoiGui?.id !== nextMsg.nguoiGui?.id;
//   }
//
//   shouldShowDate(index: number): boolean {
//     if (index === 0) return true;
//     const currentMsg = this.messages[index];
//     const prevMsg = this.messages[index - 1];
//     const currentDate = new Date(currentMsg.guiLuc).toDateString();
//     const prevDate = new Date(prevMsg.guiLuc).toDateString();
//     return currentDate !== prevDate;
//   }
//
//   private scrollToBottom(): void {
//     setTimeout(() => {
//       const container = document.querySelector('.messages-container');
//       if (container) {
//         container.scrollTop = container.scrollHeight;
//       }
//     }, 100);
//   }
// }
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
  selector: 'app-chat-main',
  standalone: true,
  imports: [CommonModule, FormsModule, PickerComponent],
  templateUrl: './chat-main.html',
  styleUrl: './chat-main.scss',
})
export class ChatMainComponent extends Base implements OnInit, OnDestroy {

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


  select_partner(item: ChatFriendItemResponse): void {
    if (this.active_partner?.partner_id === item.partner_id) return;

    this.active_partner = item;
    this.messages = [];
    this.messages_page = 0;
    this.messages_has_more = true;
    item.unread_count = 0;

    this.load_messages(0);
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
