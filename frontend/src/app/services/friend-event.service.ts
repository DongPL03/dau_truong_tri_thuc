import { Injectable } from '@angular/core';
import { Observable, Subject } from 'rxjs';

export type FriendEventType =
  | 'FRIEND_REQUEST_RECEIVED'
  | 'FRIEND_REQUEST_ACCEPTED'
  | 'FRIEND_REQUEST_DECLINED'
  | 'FRIEND_REMOVED';

export interface FriendEvent {
  type: FriendEventType;
  userId?: number;
  userName?: string;
}

/**
 * Service để emit các event liên quan đến Friend
 * Các component có thể subscribe để auto-refresh data
 */
@Injectable({ providedIn: 'root' })
export class FriendEventService {
  private eventSubject = new Subject<FriendEvent>();

  events$: Observable<FriendEvent> = this.eventSubject.asObservable();

  /**
   * Emit một friend event
   */
  emit(event: FriendEvent): void {
    this.eventSubject.next(event);
  }

  /**
   * Parse notification và emit event tương ứng
   * @param notif NotificationResponse object từ WebSocket
   */
  handleNotification(notif: { loai: string; metadata?: string | null }): void {
    if (!notif.metadata) return;

    try {
      const meta = JSON.parse(notif.metadata);
      const type = meta.type as string;

      switch (type) {
        case 'FRIEND_REQUEST':
          this.emit({
            type: 'FRIEND_REQUEST_RECEIVED',
            userId: meta.from_user_id,
            userName: meta.from_ho_ten,
          });
          break;
        case 'FRIEND_ACCEPTED':
          this.emit({
            type: 'FRIEND_REQUEST_ACCEPTED',
            userId: meta.accepter_id,
            userName: meta.accepter_ho_ten,
          });
          break;
        case 'FRIEND_DECLINED':
          this.emit({
            type: 'FRIEND_REQUEST_DECLINED',
            userId: meta.decliner_id,
            userName: meta.decliner_ho_ten,
          });
          break;
      }
    } catch (e) {
      console.error('Failed to parse friend notification metadata', e);
    }
  }
}
