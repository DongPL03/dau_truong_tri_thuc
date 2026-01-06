import { Injectable } from '@angular/core';
import { BehaviorSubject } from 'rxjs';
import { FriendSummaryResponse } from '../responses/banbe/friend_summary_response';

@Injectable({ providedIn: 'root' })
export class ChatBoxService {
  private maxChatBoxes = 3;

  private openChatsSubject = new BehaviorSubject<FriendSummaryResponse[]>([]);
  openChats$ = this.openChatsSubject.asObservable();

  get openChats(): FriendSummaryResponse[] {
    return this.openChatsSubject.value;
  }

  openChat(friend: FriendSummaryResponse): void {
    const chats = this.openChatsSubject.value;

    // Check if already open
    if (chats.some((c) => c.user_id === friend.user_id)) {
      return;
    }

    // Add to beginning, limit to max
    const newChats = [friend, ...chats].slice(0, this.maxChatBoxes);
    this.openChatsSubject.next(newChats);
  }

  closeChat(userId: number): void {
    const chats = this.openChatsSubject.value.filter((c) => c.user_id !== userId);
    this.openChatsSubject.next(chats);
  }

  closeAll(): void {
    this.openChatsSubject.next([]);
  }
}
