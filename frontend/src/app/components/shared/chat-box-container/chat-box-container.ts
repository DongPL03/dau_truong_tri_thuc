import { CommonModule } from '@angular/common';
import { Component, OnDestroy, OnInit } from '@angular/core';
import { Subject, takeUntil } from 'rxjs';
import { FriendSummaryResponse } from '../../../responses/banbe/friend_summary_response';
import { ChatBoxService } from '../../../services/chat-box.service';
import { ChatBoxComponent } from '../chat-box/chat-box';

@Component({
  selector: 'app-chat-box-container',
  standalone: true,
  imports: [CommonModule, ChatBoxComponent],
  template: `
    <div class="chat-boxes-container">
      @for (friend of openChats; track friend.user_id; let i = $index) {
      <app-chat-box [friend]="friend" [positionIndex]="i" (close)="onClose(friend.user_id)" />
      }
    </div>
  `,
  styles: [
    `
      .chat-boxes-container {
        position: fixed;
        bottom: 0;
        right: 0;
        z-index: 1000;
        pointer-events: none;
        display: flex;
        flex-direction: row-reverse;
        align-items: flex-end;
        gap: 10px;
        padding-right: 10px;

        app-chat-box {
          pointer-events: auto;
        }
      }
    `,
  ],
})
export class ChatBoxContainerComponent implements OnInit, OnDestroy {
  private destroy$ = new Subject<void>();
  openChats: FriendSummaryResponse[] = [];

  constructor(private chatBoxService: ChatBoxService) {}

  ngOnInit(): void {
    this.chatBoxService.openChats$.pipe(takeUntil(this.destroy$)).subscribe((chats) => {
      this.openChats = chats;
    });
  }

  ngOnDestroy(): void {
    this.destroy$.next();
    this.destroy$.complete();
  }

  onClose(userId: number): void {
    this.chatBoxService.closeChat(userId);
  }
}
