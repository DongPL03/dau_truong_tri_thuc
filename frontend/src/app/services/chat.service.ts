// import { HttpClient } from '@angular/common/http';
// import { Injectable, OnDestroy, inject } from '@angular/core';
// import { Client, IMessage, StompSubscription } from '@stomp/stompjs';
// import { BehaviorSubject, Observable, Subject } from 'rxjs';
// import SockJS from 'sockjs-client';
// import { environment } from '../environments/environment';
// import {
//   CapNhatPhongChatDTO,
//   ChatApiResponse,
//   ChatPageResponse,
//   ChatWebSocketMessage,
//   GuiTinNhanDTO,
//   LoaiTinNhan,
//   PhongChat,
//   TaoPhongChatDTO,
//   TinNhan,
//   TypingUser,
// } from '../models/chat';
// import { HttpUtilService } from './http.util.service';
// import { TokenService } from './token.service';
//
// @Injectable({
//   providedIn: 'root',
// })
// export class ChatService implements OnDestroy {
//   private http = inject(HttpClient);
//   private httpUtil = inject(HttpUtilService);
//   private tokenService = inject(TokenService);
//
//   private apiUrl = `${environment.apiBaseUrl}/chat`;
//   private wsUrl = environment.wsUrl || 'http://localhost:8088/ws';
//
//   private stompClient: Client | null = null;
//   private subscriptions: Map<string, StompSubscription> = new Map();
//   private reconnectAttempts = 0;
//   private maxReconnectAttempts = 5;
//
//   // Observables
//   private messagesSubject = new Subject<ChatWebSocketMessage>();
//   public messages$ = this.messagesSubject.asObservable();
//
//   private connectionStatusSubject = new BehaviorSubject<boolean>(false);
//   public connectionStatus$ = this.connectionStatusSubject.asObservable();
//
//   private typingUsersSubject = new BehaviorSubject<Map<number, TypingUser[]>>(new Map());
//   public typingUsers$ = this.typingUsersSubject.asObservable();
//
//   private unreadCountSubject = new BehaviorSubject<number>(0);
//   public unreadCount$ = this.unreadCountSubject.asObservable();
//
//   private typingTimeout: any = null;
//
//   ngOnDestroy(): void {
//     this.disconnect();
//   }
//
//   // ============== WEBSOCKET ==============
//
//   connect(): void {
//     if (this.stompClient?.connected) {
//       return;
//     }
//
//     const token = this.tokenService.getAccessToken();
//     if (!token) {
//       console.warn('No token available for WebSocket connection');
//       return;
//     }
//
//     this.stompClient = new Client({
//       webSocketFactory: () => new SockJS(this.wsUrl),
//       connectHeaders: {
//         Authorization: `Bearer ${token}`,
//       },
//       debug: (str) => {
//         if (!environment.production) {
//           console.log('[STOMP]', str);
//         }
//       },
//       reconnectDelay: 5000,
//       heartbeatIncoming: 4000,
//       heartbeatOutgoing: 4000,
//     });
//
//     this.stompClient.onConnect = () => {
//       console.log('Chat WebSocket connected');
//       this.connectionStatusSubject.next(true);
//       this.reconnectAttempts = 0;
//
//       // Subscribe to user notifications
//       this.subscribeToUserNotifications();
//     };
//
//     this.stompClient.onDisconnect = () => {
//       console.log('Chat WebSocket disconnected');
//       this.connectionStatusSubject.next(false);
//     };
//
//     this.stompClient.onStompError = (frame) => {
//       console.error('STOMP error:', frame);
//       this.connectionStatusSubject.next(false);
//     };
//
//     this.stompClient.activate();
//   }
//
//   disconnect(): void {
//     this.subscriptions.forEach((sub) => sub.unsubscribe());
//     this.subscriptions.clear();
//
//     if (this.stompClient) {
//       this.stompClient.deactivate();
//       this.stompClient = null;
//     }
//
//     this.connectionStatusSubject.next(false);
//   }
//
//   private subscribeToUserNotifications(): void {
//     if (!this.stompClient?.connected) return;
//
//     const userId = this.tokenService.getUserId();
//     if (!userId) return;
//
//     // Subscribe to personal notifications
//     const sub = this.stompClient.subscribe(
//       `/user/${userId}/queue/notifications`,
//       (message: IMessage) => {
//         try {
//           const wsMessage: ChatWebSocketMessage = JSON.parse(message.body);
//           this.messagesSubject.next(wsMessage);
//
//           if (wsMessage.type === 'NEW_CHAT_MESSAGE') {
//             this.incrementUnreadCount();
//           }
//         } catch (e) {
//           console.error('Error parsing notification:', e);
//         }
//       }
//     );
//
//     this.subscriptions.set('user-notifications', sub);
//   }
//
//   subscribeToRoom(roomId: number): void {
//     if (!this.stompClient?.connected) {
//       console.warn('WebSocket not connected, cannot subscribe to room');
//       return;
//     }
//
//     const topic = `/topic/chat/${roomId}`;
//
//     if (this.subscriptions.has(topic)) {
//       return; // Already subscribed
//     }
//
//     const sub = this.stompClient.subscribe(topic, (message: IMessage) => {
//       try {
//         const wsMessage: ChatWebSocketMessage = JSON.parse(message.body);
//         this.handleRoomMessage(roomId, wsMessage);
//       } catch (e) {
//         console.error('Error parsing room message:', e);
//       }
//     });
//
//     this.subscriptions.set(topic, sub);
//   }
//
//   unsubscribeFromRoom(roomId: number): void {
//     const topic = `/topic/chat/${roomId}`;
//     const sub = this.subscriptions.get(topic);
//
//     if (sub) {
//       sub.unsubscribe();
//       this.subscriptions.delete(topic);
//     }
//   }
//
//   private handleRoomMessage(roomId: number, message: ChatWebSocketMessage): void {
//     this.messagesSubject.next(message);
//
//     switch (message.type) {
//       case 'USER_TYPING':
//         this.addTypingUser(roomId, message.data);
//         break;
//       case 'USER_STOP_TYPING':
//         this.removeTypingUser(roomId, message.data.userId);
//         break;
//       case 'USER_READ':
//         // Handle read receipts if needed
//         break;
//     }
//   }
//
//   // Typing indicators
//   sendTyping(roomId: number): void {
//     if (!this.stompClient?.connected) return;
//
//     this.stompClient.publish({
//       destination: `/app/chat.typing/${roomId}`,
//     });
//
//     // Auto stop typing after 3 seconds
//     if (this.typingTimeout) {
//       clearTimeout(this.typingTimeout);
//     }
//     this.typingTimeout = setTimeout(() => {
//       this.sendStopTyping(roomId);
//     }, 3000);
//   }
//
//   sendStopTyping(roomId: number): void {
//     if (!this.stompClient?.connected) return;
//
//     this.stompClient.publish({
//       destination: `/app/chat.stopTyping/${roomId}`,
//     });
//
//     if (this.typingTimeout) {
//       clearTimeout(this.typingTimeout);
//       this.typingTimeout = null;
//     }
//   }
//
//   private addTypingUser(roomId: number, data: any): void {
//     const userId = this.tokenService.getUserId();
//     if (data.userId === userId) return; // Don't show own typing
//
//     const current = this.typingUsersSubject.value;
//     const roomTyping = current.get(roomId) || [];
//
//     if (!roomTyping.find((u) => u.userId === data.userId)) {
//       roomTyping.push({ userId: data.userId, userName: data.userName });
//       current.set(roomId, roomTyping);
//       this.typingUsersSubject.next(new Map(current));
//     }
//   }
//
//   private removeTypingUser(roomId: number, userId: number): void {
//     const current = this.typingUsersSubject.value;
//     const roomTyping = current.get(roomId) || [];
//
//     const filtered = roomTyping.filter((u) => u.userId !== userId);
//     current.set(roomId, filtered);
//     this.typingUsersSubject.next(new Map(current));
//   }
//
//   // Send message via WebSocket
//   sendMessageWs(dto: GuiTinNhanDTO): void {
//     if (!this.stompClient?.connected) {
//       console.warn('WebSocket not connected, using HTTP fallback');
//       return;
//     }
//
//     this.stompClient.publish({
//       destination: '/app/chat.send',
//       body: JSON.stringify(dto),
//     });
//   }
//
//   markAsReadWs(roomId: number): void {
//     if (!this.stompClient?.connected) return;
//
//     this.stompClient.publish({
//       destination: `/app/chat.read/${roomId}`,
//     });
//   }
//
//   // Unread count
//   private incrementUnreadCount(): void {
//     this.unreadCountSubject.next(this.unreadCountSubject.value + 1);
//   }
//
//   resetUnreadCount(): void {
//     this.unreadCountSubject.next(0);
//   }
//
//   // ============== HTTP APIs ==============
//
//   // Phòng chat
//   createRoom(dto: TaoPhongChatDTO): Observable<ChatApiResponse<PhongChat>> {
//     return this.http.post<ChatApiResponse<PhongChat>>(`${this.apiUrl}/rooms`, dto, {
//       headers: this.httpUtil.createAuthHeaders(),
//     });
//   }
//
//   getOrCreatePrivateChat(userId: number): Observable<ChatApiResponse<PhongChat>> {
//     return this.http.post<ChatApiResponse<PhongChat>>(
//       `${this.apiUrl}/rooms/private/${userId}`,
//       {},
//       { headers: this.httpUtil.createAuthHeaders() }
//     );
//   }
//
//   getRooms(
//     page: number = 0,
//     limit: number = 20
//   ): Observable<ChatApiResponse<ChatPageResponse<PhongChat>>> {
//     return this.http.get<ChatApiResponse<ChatPageResponse<PhongChat>>>(
//       `${this.apiUrl}/rooms?page=${page}&limit=${limit}`,
//       { headers: this.httpUtil.createAuthHeaders() }
//     );
//   }
//
//   getPinnedRooms(): Observable<ChatApiResponse<PhongChat[]>> {
//     return this.http.get<ChatApiResponse<PhongChat[]>>(`${this.apiUrl}/rooms/pinned`, {
//       headers: this.httpUtil.createAuthHeaders(),
//     });
//   }
//
//   getRoomDetail(roomId: number): Observable<ChatApiResponse<PhongChat>> {
//     return this.http.get<ChatApiResponse<PhongChat>>(`${this.apiUrl}/rooms/${roomId}`, {
//       headers: this.httpUtil.createAuthHeaders(),
//     });
//   }
//
//   updateRoom(roomId: number, dto: CapNhatPhongChatDTO): Observable<ChatApiResponse<PhongChat>> {
//     return this.http.put<ChatApiResponse<PhongChat>>(`${this.apiUrl}/rooms/${roomId}`, dto, {
//       headers: this.httpUtil.createAuthHeaders(),
//     });
//   }
//
//   leaveRoom(roomId: number): Observable<ChatApiResponse<void>> {
//     return this.http.post<ChatApiResponse<void>>(
//       `${this.apiUrl}/rooms/${roomId}/leave`,
//       {},
//       { headers: this.httpUtil.createAuthHeaders() }
//     );
//   }
//
//   deleteRoom(roomId: number): Observable<ChatApiResponse<void>> {
//     return this.http.delete<ChatApiResponse<void>>(`${this.apiUrl}/rooms/${roomId}`, {
//       headers: this.httpUtil.createAuthHeaders(),
//     });
//   }
//
//   togglePinRoom(roomId: number): Observable<ChatApiResponse<PhongChat>> {
//     return this.http.post<ChatApiResponse<PhongChat>>(
//       `${this.apiUrl}/rooms/${roomId}/pin`,
//       {},
//       { headers: this.httpUtil.createAuthHeaders() }
//     );
//   }
//
//   toggleMuteRoom(roomId: number): Observable<ChatApiResponse<PhongChat>> {
//     return this.http.post<ChatApiResponse<PhongChat>>(
//       `${this.apiUrl}/rooms/${roomId}/mute`,
//       {},
//       { headers: this.httpUtil.createAuthHeaders() }
//     );
//   }
//
//   searchRooms(
//     keyword: string,
//     page: number = 0,
//     limit: number = 20
//   ): Observable<ChatApiResponse<ChatPageResponse<PhongChat>>> {
//     return this.http.get<ChatApiResponse<ChatPageResponse<PhongChat>>>(
//       `${this.apiUrl}/rooms/search?keyword=${encodeURIComponent(
//         keyword
//       )}&page=${page}&limit=${limit}`,
//       { headers: this.httpUtil.createAuthHeaders() }
//     );
//   }
//
//   getTotalUnreadCount(): Observable<ChatApiResponse<number>> {
//     return this.http.get<ChatApiResponse<number>>(`${this.apiUrl}/unread-count`, {
//       headers: this.httpUtil.createAuthHeaders(),
//     });
//   }
//
//   // Tin nhắn
//   sendMessage(dto: GuiTinNhanDTO): Observable<ChatApiResponse<TinNhan>> {
//     return this.http.post<ChatApiResponse<TinNhan>>(`${this.apiUrl}/messages`, dto, {
//       headers: this.httpUtil.createAuthHeaders(),
//     });
//   }
//
//   getMessages(
//     roomId: number,
//     page: number = 0,
//     limit: number = 30
//   ): Observable<ChatApiResponse<ChatPageResponse<TinNhan>>> {
//     return this.http.get<ChatApiResponse<ChatPageResponse<TinNhan>>>(
//       `${this.apiUrl}/rooms/${roomId}/messages?page=${page}&limit=${limit}`,
//       { headers: this.httpUtil.createAuthHeaders() }
//     );
//   }
//
//   getMessagesBefore(
//     roomId: number,
//     messageId: number,
//     limit: number = 30
//   ): Observable<ChatApiResponse<ChatPageResponse<TinNhan>>> {
//     return this.http.get<ChatApiResponse<ChatPageResponse<TinNhan>>>(
//       `${this.apiUrl}/rooms/${roomId}/messages/before/${messageId}?limit=${limit}`,
//       { headers: this.httpUtil.createAuthHeaders() }
//     );
//   }
//
//   searchMessages(
//     roomId: number,
//     keyword: string,
//     page: number = 0,
//     limit: number = 20
//   ): Observable<ChatApiResponse<ChatPageResponse<TinNhan>>> {
//     return this.http.get<ChatApiResponse<ChatPageResponse<TinNhan>>>(
//       `${this.apiUrl}/rooms/${roomId}/messages/search?keyword=${encodeURIComponent(
//         keyword
//       )}&page=${page}&limit=${limit}`,
//       { headers: this.httpUtil.createAuthHeaders() }
//     );
//   }
//
//   editMessage(messageId: number, noiDung: string): Observable<ChatApiResponse<TinNhan>> {
//     return this.http.put<ChatApiResponse<TinNhan>>(
//       `${this.apiUrl}/messages/${messageId}`,
//       noiDung,
//       {
//         headers: this.httpUtil.createAuthHeaders().set('Content-Type', 'text/plain'),
//       }
//     );
//   }
//
//   deleteMessage(messageId: number): Observable<ChatApiResponse<void>> {
//     return this.http.delete<ChatApiResponse<void>>(`${this.apiUrl}/messages/${messageId}`, {
//       headers: this.httpUtil.createAuthHeaders(),
//     });
//   }
//
//   togglePinMessage(messageId: number): Observable<ChatApiResponse<TinNhan>> {
//     return this.http.post<ChatApiResponse<TinNhan>>(
//       `${this.apiUrl}/messages/${messageId}/pin`,
//       {},
//       { headers: this.httpUtil.createAuthHeaders() }
//     );
//   }
//
//   getPinnedMessages(roomId: number): Observable<ChatApiResponse<TinNhan[]>> {
//     return this.http.get<ChatApiResponse<TinNhan[]>>(
//       `${this.apiUrl}/rooms/${roomId}/messages/pinned`,
//       {
//         headers: this.httpUtil.createAuthHeaders(),
//       }
//     );
//   }
//
//   markAsRead(roomId: number): Observable<ChatApiResponse<void>> {
//     return this.http.post<ChatApiResponse<void>>(
//       `${this.apiUrl}/rooms/${roomId}/read`,
//       {},
//       { headers: this.httpUtil.createAuthHeaders() }
//     );
//   }
//
//   // Upload file cho chat
//   uploadChatFile(file: File): Observable<ChatApiResponse<string>> {
//     const formData = new FormData();
//     formData.append('file', file);
//
//     return this.http.post<ChatApiResponse<string>>(`${this.apiUrl}/upload`, formData, {
//       headers: this.httpUtil.createAuthHeaders().delete('Content-Type'),
//     });
//   }
//
//   // Helper để format file size
//   formatFileSize(bytes: number): string {
//     if (bytes === 0) return '0 Bytes';
//     const k = 1024;
//     const sizes = ['Bytes', 'KB', 'MB', 'GB'];
//     const i = Math.floor(Math.log(bytes) / Math.log(k));
//     return parseFloat((bytes / Math.pow(k, i)).toFixed(2)) + ' ' + sizes[i];
//   }
//
//   // Helper để lấy icon cho loại tin nhắn
//   getMessageTypeIcon(loai: LoaiTinNhan): string {
//     const icons: Record<LoaiTinNhan, string> = {
//       [LoaiTinNhan.VAN_BAN]: 'fa-comment',
//       [LoaiTinNhan.HINH_ANH]: 'fa-image',
//       [LoaiTinNhan.TAP_TIN]: 'fa-file',
//       [LoaiTinNhan.AM_THANH]: 'fa-microphone',
//       [LoaiTinNhan.HE_THONG]: 'fa-info-circle',
//       [LoaiTinNhan.STICKER]: 'fa-smile',
//       [LoaiTinNhan.EMOJI]: 'fa-heart',
//     };
//     return icons[loai] || 'fa-comment';
//   }
// }
// src/app/services/chat.service.ts
import { HttpClient } from '@angular/common/http';
import { inject, Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { SendMessageDto } from '../dtos/tinnhan/send-message-dto';
import { environment } from '../environments/environment';
import { ChatInboxItemResponse } from '../responses/chat/chat-inbox-items-response'; // 👈 THÊM DÒNG NÀY
import { ChatMessageResponse } from '../responses/chat/chat-message-response';
import { PageResponse } from '../responses/page-response';
import { ResponseObject } from '../responses/response-object';
import { HttpUtilService } from './http.util.service';

@Injectable({ providedIn: 'root' })
export class ChatService {
  private http = inject(HttpClient);
  private httpUtil = inject(HttpUtilService);
  private readonly api = `${environment.apiBaseUrl}/chat`;

  sendMessage(dto: SendMessageDto): Observable<ResponseObject<ChatMessageResponse>> {
    return this.http.post<ResponseObject<ChatMessageResponse>>(`${this.api}/send`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  getConversation(
    friend_user_id: number,
    page = 0,
    limit = 20
  ): Observable<ResponseObject<PageResponse<ChatMessageResponse>>> {
    const params = { friend_user_id, page, limit };
    return this.http.get<ResponseObject<PageResponse<ChatMessageResponse>>>(
      `${this.api}/conversation`,
      { params, headers: this.httpUtil.createAuthHeaders() }
    );
  }

  // 👇 ĐỔI GENERIC Ở ĐÂY
  getInbox(page = 0, limit = 20): Observable<ResponseObject<PageResponse<ChatInboxItemResponse>>> {
    const params = { page, limit };
    return this.http.get<ResponseObject<PageResponse<ChatInboxItemResponse>>>(`${this.api}/inbox`, {
      params,
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * Lấy danh sách cuộc trò chuyện gần đây với thông tin partner
   * GET /chat/inbox-list?limit=10
   */
  getInboxList(limit = 10): Observable<ResponseObject<ChatInboxItemResponse[]>> {
    const params = { limit };
    return this.http.get<ResponseObject<ChatInboxItemResponse[]>>(`${this.api}/inbox-list`, {
      params,
      headers: this.httpUtil.createAuthHeaders(),
    });
  }
}
