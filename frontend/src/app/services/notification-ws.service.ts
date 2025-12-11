import {Injectable, OnDestroy} from '@angular/core';
import {Client, IMessage} from '@stomp/stompjs'; // Chỉ cần Client và IMessage
import SockJS from 'sockjs-client';
import {Observable, Subject} from 'rxjs';
import {environment} from '../environments/environment';
import {NotificationResponse} from '../responses/notification/notification-response';

@Injectable({providedIn: 'root'})
export class NotificationWsService implements OnDestroy {
  // 1. Dùng đúng kiểu Client chuẩn
  private readonly client: Client;
  private notification_subject = new Subject<NotificationResponse>();
  notifications$: Observable<NotificationResponse> = this.notification_subject.asObservable();

  constructor() {
    // Khởi tạo client rỗng trước (hoặc để trong connect tùy logic)
    // Ở đây mình khởi tạo cấu hình cơ bản trước
    this.client = new Client({
      // Cấu hình reconnect: Tự động thử lại sau 5s nếu mất mạng
      reconnectDelay: 5000,
      heartbeatIncoming: 4000,
      heartbeatOutgoing: 4000,
    });
  }

  connect(user_id: number): void {
    // Nếu đang active thì không connect lại
    if (this.client && this.client.active) {
      return;
    }

    const http_base = environment.apiBaseUrl.replace('/api/v1', '');
    const socketUrl = `${http_base}/ws`;

    // 2. Cấu hình Client
    this.client.configure({
      // Vì backend Spring Boot dùng SockJS, ta phải dùng webSocketFactory
      // Nếu backend hỗ trợ WS thuần thì dùng brokerURL: 'ws://...'
      webSocketFactory: () => new SockJS(socketUrl),

      // Debug log (bỏ comment nếu muốn xem log)
      // debug: (str) => console.log(str),

      // 3. Xử lý khi kết nối thành công (thay cho callback cũ)
      onConnect: (frame) => {
        console.log('Connected to WS: ' + frame);
        const dest = `/topic/notifications/${user_id}`;

        // Subscribe ngay trong onConnect
        this.client.subscribe(dest, (msg: IMessage) => {
          if (!msg.body) return;
          try {
            const data = JSON.parse(msg.body) as NotificationResponse;
            this.notification_subject.next(data);
          } catch (e) {
            console.error('Parse notification ws error', e);
          }
        });
      },

      // Xử lý lỗi STOMP
      onStompError: (frame) => {
        console.error('Broker reported error: ' + frame.headers['message']);
        console.error('Additional details: ' + frame.body);
      },

      // Xử lý khi đóng kết nối WebSocket
      onWebSocketClose: () => {
        console.log('WebSocket connection closed');
      }
    });

    // 4. Kích hoạt kết nối
    this.client.activate();
  }

  ngOnDestroy(): void {
    // 5. Hủy kích hoạt đúng chuẩn
    if (this.client) {
      this.client.deactivate().then(r => {
      }).catch(err => {
        console.error('Error during WS deactivation', err);
      });
    }
  }
}
