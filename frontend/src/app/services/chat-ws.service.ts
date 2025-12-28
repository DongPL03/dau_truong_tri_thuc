import {Injectable} from '@angular/core';
import {Client, IMessage, Stomp} from '@stomp/stompjs';
import SockJS from 'sockjs-client';
import {Subject} from 'rxjs';
import {environment} from '../environments/environment';
import {ChatMessageResponse} from '../responses/chat/chat-message-response';

@Injectable({providedIn: 'root'})
export class ChatWsService {
  private client?: Client;
  private connected = false;

  private messageSubject = new Subject<ChatMessageResponse>();
  messages$ = this.messageSubject.asObservable();

  connect(userId: number): void {
    if (this.connected) {
      return;
    }

    const baseHttp = environment.apiBaseUrl.replace('/api/v1', '');
    const wsUrl = (environment as any).wsBaseUrl || `${baseHttp}/ws`;

    const socket = new SockJS(wsUrl);
    const client = Stomp.over(socket);
    client.debug = () => {
    };

    client.onConnect = () => {
      this.connected = true;

      client.subscribe(`/topic/chat/user/${userId}`, (msg: IMessage) => {
        if (!msg.body) {
          return;
        }
        try {
          const payload: ChatMessageResponse = JSON.parse(msg.body);
          this.messageSubject.next(payload);
        } catch (e) {
          console.error('WS chat parse error', e);
        }
      });
    };

    client.onStompError = (frame) => {
      console.error('STOMP chat error', frame);
    };

    client.activate();
    this.client = client;
  }

  disconnect(): void {
    if (this.client && this.connected) {
      this.client.deactivate().then(r => {
      }).catch(err => {
        console.error('WS chat disconnect error', err);
      });
    }
    this.connected = false;
  }
}
