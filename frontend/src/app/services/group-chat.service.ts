import { HttpClient } from '@angular/common/http';
import { inject, Injectable, OnDestroy } from '@angular/core';
import { Client, IMessage, StompSubscription } from '@stomp/stompjs';
import { BehaviorSubject, Observable, Subject } from 'rxjs';
import SockJS from 'sockjs-client';
import { environment } from '../environments/environment';
import {
  CapNhatPhongChatDTO,
  GuiTinNhanDTO,
  LoaiTinNhan,
  PhongChat,
  TaoPhongChatDTO,
  TinNhan,
  TypingUser,
} from '../models/chat';
import { PageResponse } from '../responses/page-response';
import { ResponseObject } from '../responses/response-object';
import { HttpUtilService } from './http.util.service';
import { TokenService } from './token.service';

export interface ChatWebSocketMessage {
  type:
    | 'NEW_CHAT_MESSAGE'
    | 'MESSAGE_EDITED'
    | 'MESSAGE_DELETED'
    | 'USER_TYPING'
    | 'USER_STOP_TYPING'
    | 'USER_READ'
    | 'ROOM_UPDATED'
    | 'MEMBER_JOINED'
    | 'MEMBER_LEFT';
  roomId?: number;
  data?: any;
}

@Injectable({
  providedIn: 'root',
})
export class GroupChatService implements OnDestroy {
  private http = inject(HttpClient);
  private httpUtil = inject(HttpUtilService);
  private tokenService = inject(TokenService);

  private apiUrl = `${environment.apiBaseUrl}/chat`;
  private wsUrl = environment.wsUrl || 'http://localhost:8088/ws';

  private stompClient: Client | null = null;
  private subscriptions: Map<string, StompSubscription> = new Map();
  private reconnectAttempts = 0;
  private maxReconnectAttempts = 5;

  // Observables
  private messagesSubject = new Subject<ChatWebSocketMessage>();
  public messages$ = this.messagesSubject.asObservable();

  private connectionStatusSubject = new BehaviorSubject<boolean>(false);
  public connectionStatus$ = this.connectionStatusSubject.asObservable();

  private typingUsersSubject = new BehaviorSubject<Map<number, TypingUser[]>>(new Map());
  public typingUsers$ = this.typingUsersSubject.asObservable();

  private unreadCountSubject = new BehaviorSubject<number>(0);
  public unreadCount$ = this.unreadCountSubject.asObservable();

  private typingTimeout: any = null;

  ngOnDestroy(): void {
    this.disconnect();
  }

  // ============== WEBSOCKET ==============

  connect(): void {
    if (this.stompClient?.connected) {
      return;
    }

    const token = this.tokenService.getAccessToken();
    if (!token) {
      console.warn('No token available for WebSocket connection');
      return;
    }

    this.stompClient = new Client({
      webSocketFactory: () => new SockJS(this.wsUrl),
      connectHeaders: {
        Authorization: `Bearer ${token}`,
      },
      debug: (str) => {
        if (!environment.production) {
          console.log('[STOMP]', str);
        }
      },
      reconnectDelay: 5000,
      heartbeatIncoming: 4000,
      heartbeatOutgoing: 4000,
    });

    this.stompClient.onConnect = () => {
      console.log('Group Chat WebSocket connected');
      this.connectionStatusSubject.next(true);
      this.reconnectAttempts = 0;

      // Subscribe to user notifications
      this.subscribeToUserNotifications();
    };

    this.stompClient.onDisconnect = () => {
      console.log('Group Chat WebSocket disconnected');
      this.connectionStatusSubject.next(false);
    };

    this.stompClient.onStompError = (frame) => {
      console.error('STOMP error:', frame);
      this.connectionStatusSubject.next(false);
    };

    this.stompClient.activate();
  }

  disconnect(): void {
    this.subscriptions.forEach((sub) => sub.unsubscribe());
    this.subscriptions.clear();

    if (this.stompClient) {
      this.stompClient.deactivate();
      this.stompClient = null;
    }

    this.connectionStatusSubject.next(false);
  }

  private subscribeToUserNotifications(): void {
    if (!this.stompClient?.connected) return;

    const userId = this.tokenService.getUserId();
    if (!userId) return;

    // Subscribe to personal notifications
    const sub = this.stompClient.subscribe(
      `/user/${userId}/queue/notifications`,
      (message: IMessage) => {
        try {
          const wsMessage: ChatWebSocketMessage = JSON.parse(message.body);
          this.messagesSubject.next(wsMessage);

          if (wsMessage.type === 'NEW_CHAT_MESSAGE') {
            this.incrementUnreadCount();
          }
        } catch (e) {
          console.error('Error parsing notification:', e);
        }
      }
    );

    this.subscriptions.set('user-notifications', sub);
  }

  subscribeToRoom(roomId: number): void {
    if (!this.stompClient?.connected) {
      console.warn('WebSocket not connected, cannot subscribe to room');
      return;
    }

    const topic = `/topic/chat/${roomId}`;

    if (this.subscriptions.has(topic)) {
      return; // Already subscribed
    }

    const sub = this.stompClient.subscribe(topic, (message: IMessage) => {
      try {
        const wsMessage: ChatWebSocketMessage = JSON.parse(message.body);
        this.handleRoomMessage(roomId, wsMessage);
      } catch (e) {
        console.error('Error parsing room message:', e);
      }
    });

    this.subscriptions.set(topic, sub);
  }

  unsubscribeFromRoom(roomId: number): void {
    const topic = `/topic/chat/${roomId}`;
    const sub = this.subscriptions.get(topic);

    if (sub) {
      sub.unsubscribe();
      this.subscriptions.delete(topic);
    }
  }

  private handleRoomMessage(roomId: number, message: ChatWebSocketMessage): void {
    this.messagesSubject.next(message);

    switch (message.type) {
      case 'USER_TYPING':
        this.addTypingUser(roomId, message.data);
        break;
      case 'USER_STOP_TYPING':
        this.removeTypingUser(roomId, message.data.userId);
        break;
      case 'USER_READ':
        // Handle read receipts if needed
        break;
    }
  }

  // Typing indicators
  sendTyping(roomId: number): void {
    if (!this.stompClient?.connected) return;

    this.stompClient.publish({
      destination: `/app/chat.typing/${roomId}`,
      body: JSON.stringify({ isTyping: true }),
    });

    // Auto stop typing after 3 seconds
    if (this.typingTimeout) {
      clearTimeout(this.typingTimeout);
    }

    this.typingTimeout = setTimeout(() => {
      this.sendStopTyping(roomId);
    }, 3000);
  }

  sendStopTyping(roomId: number): void {
    if (!this.stompClient?.connected) return;

    this.stompClient.publish({
      destination: `/app/chat.typing/${roomId}`,
      body: JSON.stringify({ isTyping: false }),
    });

    if (this.typingTimeout) {
      clearTimeout(this.typingTimeout);
      this.typingTimeout = null;
    }
  }

  private addTypingUser(roomId: number, user: TypingUser): void {
    const currentMap = this.typingUsersSubject.value;
    const roomUsers = currentMap.get(roomId) || [];

    if (!roomUsers.find((u) => u.userId === user.userId)) {
      roomUsers.push(user);
      currentMap.set(roomId, roomUsers);
      this.typingUsersSubject.next(new Map(currentMap));
    }
  }

  private removeTypingUser(roomId: number, userId: number): void {
    const currentMap = this.typingUsersSubject.value;
    const roomUsers = currentMap.get(roomId) || [];

    const filtered = roomUsers.filter((u) => u.userId !== userId);
    currentMap.set(roomId, filtered);
    this.typingUsersSubject.next(new Map(currentMap));
  }

  private incrementUnreadCount(): void {
    this.unreadCountSubject.next(this.unreadCountSubject.value + 1);
  }

  // ============== REST API ==============

  // Rooms
  createRoom(dto: TaoPhongChatDTO): Observable<ResponseObject<PhongChat>> {
    return this.http.post<ResponseObject<PhongChat>>(`${this.apiUrl}/rooms`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  getOrCreatePrivateChat(userId: number): Observable<ResponseObject<PhongChat>> {
    return this.http.post<ResponseObject<PhongChat>>(
      `${this.apiUrl}/rooms/private/${userId}`,
      {},
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  getRooms(page = 0, limit = 20): Observable<ResponseObject<PageResponse<PhongChat>>> {
    return this.http.get<ResponseObject<PageResponse<PhongChat>>>(`${this.apiUrl}/rooms`, {
      params: { page, limit },
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  getPinnedRooms(): Observable<ResponseObject<PhongChat[]>> {
    return this.http.get<ResponseObject<PhongChat[]>>(`${this.apiUrl}/rooms/pinned`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  getRoomDetail(roomId: number): Observable<ResponseObject<PhongChat>> {
    return this.http.get<ResponseObject<PhongChat>>(`${this.apiUrl}/rooms/${roomId}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  updateRoom(roomId: number, dto: CapNhatPhongChatDTO): Observable<ResponseObject<PhongChat>> {
    return this.http.put<ResponseObject<PhongChat>>(`${this.apiUrl}/rooms/${roomId}`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  deleteRoom(roomId: number): Observable<ResponseObject<void>> {
    return this.http.delete<ResponseObject<void>>(`${this.apiUrl}/rooms/${roomId}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  leaveRoom(roomId: number): Observable<ResponseObject<void>> {
    return this.http.post<ResponseObject<void>>(
      `${this.apiUrl}/rooms/${roomId}/leave`,
      {},
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  togglePinRoom(roomId: number): Observable<ResponseObject<PhongChat>> {
    return this.http.post<ResponseObject<PhongChat>>(
      `${this.apiUrl}/rooms/${roomId}/pin`,
      {},
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  toggleMuteRoom(roomId: number): Observable<ResponseObject<PhongChat>> {
    return this.http.post<ResponseObject<PhongChat>>(
      `${this.apiUrl}/rooms/${roomId}/mute`,
      {},
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  searchRooms(
    keyword: string,
    page = 0,
    limit = 20
  ): Observable<ResponseObject<PageResponse<PhongChat>>> {
    return this.http.get<ResponseObject<PageResponse<PhongChat>>>(`${this.apiUrl}/rooms/search`, {
      params: { keyword, page, limit },
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  // Messages
  sendMessage(dto: GuiTinNhanDTO): Observable<ResponseObject<TinNhan>> {
    return this.http.post<ResponseObject<TinNhan>>(`${this.apiUrl}/messages`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  getMessages(
    roomId: number,
    page = 0,
    limit = 30
  ): Observable<ResponseObject<PageResponse<TinNhan>>> {
    return this.http.get<ResponseObject<PageResponse<TinNhan>>>(
      `${this.apiUrl}/rooms/${roomId}/messages`,
      {
        params: { page, limit },
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  getMessagesBefore(
    roomId: number,
    beforeId: number,
    limit = 30
  ): Observable<ResponseObject<PageResponse<TinNhan>>> {
    return this.http.get<ResponseObject<PageResponse<TinNhan>>>(
      `${this.apiUrl}/rooms/${roomId}/messages`,
      {
        params: { before: beforeId, limit },
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  editMessage(messageId: number, noiDung: string): Observable<ResponseObject<TinNhan>> {
    return this.http.put<ResponseObject<TinNhan>>(
      `${this.apiUrl}/messages/${messageId}`,
      { noiDung },
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  deleteMessage(messageId: number): Observable<ResponseObject<void>> {
    return this.http.delete<ResponseObject<void>>(`${this.apiUrl}/messages/${messageId}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  togglePinMessage(roomId: number, messageId: number): Observable<ResponseObject<TinNhan>> {
    return this.http.post<ResponseObject<TinNhan>>(
      `${this.apiUrl}/rooms/${roomId}/messages/${messageId}/pin`,
      {},
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  // Read status
  markAsRead(roomId: number): Observable<ResponseObject<void>> {
    return this.http.post<ResponseObject<void>>(
      `${this.apiUrl}/rooms/${roomId}/read`,
      {},
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  // Members
  addMembers(roomId: number, memberIds: number[]): Observable<ResponseObject<PhongChat>> {
    return this.http.post<ResponseObject<PhongChat>>(
      `${this.apiUrl}/rooms/${roomId}/members`,
      { memberIds },
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  removeMember(roomId: number, memberId: number): Observable<ResponseObject<void>> {
    return this.http.delete<ResponseObject<void>>(
      `${this.apiUrl}/rooms/${roomId}/members/${memberId}`,
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  updateMemberRole(
    roomId: number,
    memberId: number,
    role: string
  ): Observable<ResponseObject<void>> {
    return this.http.put<ResponseObject<void>>(
      `${this.apiUrl}/rooms/${roomId}/members/${memberId}/role`,
      { role },
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  // Unread count
  getTotalUnread(): Observable<ResponseObject<number>> {
    return this.http.get<ResponseObject<number>>(`${this.apiUrl}/unread-count`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  // Helpers
  getMessageTypeIcon(loai: LoaiTinNhan): string {
    const icons: Record<LoaiTinNhan, string> = {
      [LoaiTinNhan.VAN_BAN]: 'fa-comment',
      [LoaiTinNhan.HINH_ANH]: 'fa-image',
      [LoaiTinNhan.TAP_TIN]: 'fa-file',
      [LoaiTinNhan.AM_THANH]: 'fa-microphone',
      [LoaiTinNhan.HE_THONG]: 'fa-info-circle',
      [LoaiTinNhan.STICKER]: 'fa-smile',
      [LoaiTinNhan.EMOJI]: 'fa-heart',
    };
    return icons[loai] || 'fa-comment';
  }
}
