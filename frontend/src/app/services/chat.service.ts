// src/app/services/chat.service.ts
import {inject, Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {Observable} from 'rxjs';
import {environment} from '../environments/environment';
import {ResponseObject} from '../responses/response-object';
import {ChatMessageResponse} from '../responses/chat/chat-message-response';
import {ChatInboxItemResponse} from '../responses/chat/chat-inbox-items-response'; // 👈 THÊM DÒNG NÀY
import {HttpUtilService} from './http.util.service';
import {PageResponse} from '../responses/page-response';
import {SendMessageDto} from '../dtos/tinnhan/send-message-dto';

@Injectable({providedIn: 'root'})
export class ChatService {
  private http = inject(HttpClient);
  private httpUtil = inject(HttpUtilService);
  private readonly api = `${environment.apiBaseUrl}/chat`;

  sendMessage(dto: SendMessageDto): Observable<ResponseObject<ChatMessageResponse>> {
    return this.http.post<ResponseObject<ChatMessageResponse>>(
      `${this.api}/send`,
      dto,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  getConversation(friend_user_id: number, page = 0, limit = 20):
    Observable<ResponseObject<PageResponse<ChatMessageResponse>>> {

    const params = {friend_user_id, page, limit};
    return this.http.get<ResponseObject<PageResponse<ChatMessageResponse>>>(
      `${this.api}/conversation`,
      {params, headers: this.httpUtil.createAuthHeaders()}
    );
  }

  // 👇 ĐỔI GENERIC Ở ĐÂY
  getInbox(page = 0, limit = 20):
    Observable<ResponseObject<PageResponse<ChatInboxItemResponse>>> {

    const params = {page, limit};
    return this.http.get<ResponseObject<PageResponse<ChatInboxItemResponse>>>(
      `${this.api}/inbox`,
      {params, headers: this.httpUtil.createAuthHeaders()}
    );
  }
}
