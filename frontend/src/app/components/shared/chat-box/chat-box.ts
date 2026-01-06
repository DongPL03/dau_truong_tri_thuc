import { CommonModule } from '@angular/common';
import {
  Component,
  ElementRef,
  EventEmitter,
  Input,
  OnDestroy,
  OnInit,
  Output,
  ViewChild,
} from '@angular/core';
import { FormsModule } from '@angular/forms';
import { Subject, takeUntil } from 'rxjs';
import { SendMessageDto } from '../../../dtos/tinnhan/send-message-dto';
import { FriendSummaryResponse } from '../../../responses/banbe/friend_summary_response';
import { ChatMessageResponse } from '../../../responses/chat/chat-message-response';
import { ChatWsService } from '../../../services/chat-ws.service';
import { ChatService } from '../../../services/chat.service';
import { TokenService } from '../../../services/token.service';

@Component({
  selector: 'app-chat-box',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './chat-box.html',
  styleUrl: './chat-box.scss',
})
export class ChatBoxComponent implements OnInit, OnDestroy {
  @Input() friend!: FriendSummaryResponse;
  @Input() positionIndex = 0;
  @Output() close = new EventEmitter<void>();

  @ViewChild('messagesContainer') messagesContainer!: ElementRef<HTMLDivElement>;
  @ViewChild('messageInput') messageInput!: ElementRef<HTMLInputElement>;

  private destroy$ = new Subject<void>();

  messages: ChatMessageResponse[] = [];
  newMessage = '';
  loading = false;
  sending = false;
  isMinimized = false;

  currentUserId: number | null = null;

  constructor(
    private chatService: ChatService,
    private chatWsService: ChatWsService,
    private tokenService: TokenService
  ) {}

  ngOnInit(): void {
    this.currentUserId = this.tokenService.getUserId();
    this.loadMessages();
    this.subscribeToMessages();
  }

  ngOnDestroy(): void {
    this.destroy$.next();
    this.destroy$.complete();
  }

  loadMessages(): void {
    this.loading = true;
    this.chatService.getConversation(this.friend.user_id, 0, 30).subscribe({
      next: (res) => {
        this.messages = (res.data?.items || []).slice().reverse();
        this.loading = false;
        setTimeout(() => this.scrollToBottom(), 100);
      },
      error: () => {
        this.loading = false;
      },
    });
  }

  subscribeToMessages(): void {
    this.chatWsService.messages$.pipe(takeUntil(this.destroy$)).subscribe((msg) => {
      if (!msg || !this.currentUserId) return;

      // Check if message is from/to this friend
      const partnerId = msg.gui_boi_id === this.currentUserId ? msg.nhan_boi_id : msg.gui_boi_id;

      if (partnerId === this.friend.user_id) {
        this.messages.push(msg);
        setTimeout(() => this.scrollToBottom(), 100);
      }
    });
  }

  sendMessage(): void {
    const content = this.newMessage.trim();
    if (!content || this.sending) return;

    this.sending = true;
    const dto = new SendMessageDto({
      receiver_id: this.friend.user_id,
      noi_dung: content,
    });

    this.chatService.sendMessage(dto).subscribe({
      next: (res) => {
        // Thêm tin nhắn vào danh sách ngay khi gửi thành công
        if (res.data) {
          this.messages.push(res.data);
          setTimeout(() => this.scrollToBottom(), 100);
        }
        this.newMessage = '';
        this.sending = false;
        this.messageInput?.nativeElement?.focus();
      },
      error: () => {
        this.sending = false;
      },
    });
  }

  onKeyDown(event: KeyboardEvent): void {
    if (event.key === 'Enter' && !event.shiftKey) {
      event.preventDefault();
      this.sendMessage();
    }
  }

  toggleMinimize(): void {
    this.isMinimized = !this.isMinimized;
    if (!this.isMinimized) {
      setTimeout(() => this.scrollToBottom(), 100);
    }
  }

  closeChat(): void {
    this.close.emit();
  }

  scrollToBottom(): void {
    if (this.messagesContainer?.nativeElement) {
      this.messagesContainer.nativeElement.scrollTop =
        this.messagesContainer.nativeElement.scrollHeight;
    }
  }

  isMyMessage(msg: ChatMessageResponse): boolean {
    return msg.gui_boi_id === this.currentUserId;
  }

  getAvatarUrl(): string {
    return this.friend.avatar_url
      ? `http://localhost:8088/api/v1/users/profile-images/${this.friend.avatar_url}`
      : 'assets/images/default-avatar.png';
  }

  formatTime(dateString: string): string {
    const date = new Date(dateString);
    return date.toLocaleTimeString('vi-VN', { hour: '2-digit', minute: '2-digit' });
  }
}
