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
    this.client = new Client({
      reconnectDelay: 5000,
      heartbeatIncoming: 4000,
      heartbeatOutgoing: 4000,
    });
  }

  connect(user_id: number): void {
    if (this.client && this.client.active) {
      return;
    }

    const http_base = environment.apiBaseUrl.replace('/api/v1', '');
    const socketUrl = `${http_base}/ws`;

    this.client.configure({
      webSocketFactory: () => new SockJS(socketUrl),
      onConnect: (frame) => {
        console.log('Connected to WS: ' + frame);
        const dest = `/topic/notifications/${user_id}`;
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

      onStompError: (frame) => {
        console.error('Broker reported error: ' + frame.headers['message']);
        console.error('Additional details: ' + frame.body);
      },
      onWebSocketClose: () => {
        console.log('WebSocket connection closed');
      }
    });

    this.client.activate();
  }

  ngOnDestroy(): void {
    if (this.client) {
      this.client.deactivate().then(r => {
      }).catch(err => {
        console.error('Error during WS deactivation', err);
      });
    }
  }
}
