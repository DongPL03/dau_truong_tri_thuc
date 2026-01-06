import { CommonModule } from '@angular/common';
import { Component, OnDestroy, OnInit } from '@angular/core';
import { Router, RouterModule } from '@angular/router';
import { Subject, takeUntil } from 'rxjs';
import { FriendSummaryResponse } from '../../../responses/banbe/friend_summary_response';
import { ChatInboxItemResponse } from '../../../responses/chat/chat-inbox-items-response';
import { ChatBoxService } from '../../../services/chat-box.service';
import { ChatService } from '../../../services/chat.service';

@Component({
  selector: 'app-recent-chats',
  standalone: true,
  imports: [CommonModule, RouterModule],
  templateUrl: './recent-chats.html',
  styleUrl: './recent-chats.scss',
})
export class RecentChatsComponent implements OnInit, OnDestroy {
  private destroy$ = new Subject<void>();

  loading = false;
  inboxItems: ChatInboxItemResponse[] = [];
  unreadTotal = 0;
  isExpanded = false; // Toggle state

  constructor(
    private chatService: ChatService,
    private chatBoxService: ChatBoxService,
    private router: Router
  ) {}

  ngOnInit(): void {
    this.loadInbox();
  }

  ngOnDestroy(): void {
    this.destroy$.next();
    this.destroy$.complete();
  }

  toggleExpand(): void {
    this.isExpanded = !this.isExpanded;
    if (this.isExpanded && this.inboxItems.length === 0) {
      this.loadInbox();
    }
  }

  loadInbox(): void {
    this.loading = true;
    this.chatService
      .getInboxList(5)
      .pipe(takeUntil(this.destroy$))
      .subscribe({
        next: (res) => {
          this.inboxItems = res.data || [];
          this.unreadTotal = this.inboxItems.reduce((sum, item) => sum + item.unreadCount, 0);
          this.loading = false;
        },
        error: () => {
          this.loading = false;
        },
      });
  }

  openChatBox(item: ChatInboxItemResponse): void {
    const friend: FriendSummaryResponse = {
      user_id: item.partnerId,
      ho_ten: item.partnerName,
      avatar_url: item.partnerAvatarUrl,
      trang_thai: 'ONLINE',
    };
    this.chatBoxService.openChat(friend);
  }

  goToChat(): void {
    this.router.navigate(['/chat']);
  }

  getAvatarUrl(item: ChatInboxItemResponse): string {
    return item.partnerAvatarUrl
      ? `http://localhost:8088/api/v1/users/profile-images/${item.partnerAvatarUrl}`
      : 'assets/images/default-avatar.png';
  }

  formatTime(dateString: string): string {
    if (!dateString) return '';

    const date = new Date(dateString);
    const now = new Date();
    const diff = now.getTime() - date.getTime();
    const minutes = Math.floor(diff / 60000);
    const hours = Math.floor(diff / 3600000);
    const days = Math.floor(diff / 86400000);

    if (minutes < 1) return 'Vừa xong';
    if (minutes < 60) return `${minutes}p`;
    if (hours < 24) return `${hours}h`;
    if (days < 7) return `${days}d`;
    return date.toLocaleDateString('vi-VN');
  }

  truncateMessage(msg: string, maxLength: number = 25): string {
    if (!msg) return '';
    return msg.length > maxLength ? msg.substring(0, maxLength) + '...' : msg;
  }
}
