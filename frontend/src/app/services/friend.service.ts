import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from '../environments/environment';
import { BlockedUserResponse } from '../responses/banbe/blocked_user_response';
import { FriendRequestDTO } from '../responses/banbe/friend_request_dto';
import { FriendRequestItemResponse } from '../responses/banbe/friend_request_item_response';
import { FriendSuggestionResponse } from '../responses/banbe/friend_suggestion_response';
import { FriendSummaryResponse } from '../responses/banbe/friend_summary_response';
import { ResponseObject } from '../responses/response-object';
import { HttpUtilService } from './http.util.service';

export interface BlockUserDTO {
  target_user_id: number;
  ly_do?: string;
}

@Injectable({ providedIn: 'root' })
export class FriendService {
  private readonly api = `${environment.apiBaseUrl}/friends`;

  constructor(private http: HttpClient, private httpUtil: HttpUtilService) {}

  /** Gửi lời mời kết bạn */
  sendRequest(dto: FriendRequestDTO): Observable<ResponseObject<null>> {
    return this.http.post<ResponseObject<null>>(`${this.api}/request`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /** Chấp nhận lời mời */
  acceptRequest(request_id: number): Observable<ResponseObject<null>> {
    return this.http.post<ResponseObject<null>>(
      `${this.api}/requests/${request_id}/accept`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /** Từ chối lời mời */
  declineRequest(request_id: number): Observable<ResponseObject<null>> {
    return this.http.post<ResponseObject<null>>(
      `${this.api}/requests/${request_id}/decline`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /** Huỷ lời mời đã gửi */
  cancelRequest(request_id: number): Observable<ResponseObject<null>> {
    return this.http.post<ResponseObject<null>>(
      `${this.api}/requests/${request_id}/cancel`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /** Huỷ kết bạn */
  unfriend(friend_user_id: number): Observable<ResponseObject<null>> {
    return this.http.delete<ResponseObject<null>>(`${this.api}/${friend_user_id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /** Danh sách lời mời đến */
  getIncomingRequests(): Observable<ResponseObject<FriendRequestItemResponse[]>> {
    return this.http.get<ResponseObject<FriendRequestItemResponse[]>>(
      `${this.api}/requests/incoming`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /** Danh sách lời mời đã gửi */
  getOutgoingRequests(): Observable<ResponseObject<FriendRequestItemResponse[]>> {
    return this.http.get<ResponseObject<FriendRequestItemResponse[]>>(
      `${this.api}/requests/outgoing`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /** Danh sách bạn bè */
  getFriends(): Observable<ResponseObject<FriendSummaryResponse[]>> {
    return this.http.get<ResponseObject<FriendSummaryResponse[]>>(`${this.api}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  // ============== BLOCK ==============

  /** Chặn người dùng */
  blockUser(dto: BlockUserDTO): Observable<ResponseObject<null>> {
    return this.http.post<ResponseObject<null>>(`${this.api}/block`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /** Bỏ chặn người dùng */
  unblockUser(user_id: number): Observable<ResponseObject<null>> {
    return this.http.delete<ResponseObject<null>>(`${this.api}/block/${user_id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /** Danh sách người đã chặn */
  getBlockedUsers(): Observable<ResponseObject<BlockedUserResponse[]>> {
    return this.http.get<ResponseObject<BlockedUserResponse[]>>(`${this.api}/blocked`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  // ============== SUGGESTIONS ==============

  /** Gợi ý kết bạn */
  getSuggestions(limit: number = 10): Observable<ResponseObject<FriendSuggestionResponse[]>> {
    return this.http.get<ResponseObject<FriendSuggestionResponse[]>>(
      `${this.api}/suggestions?limit=${limit}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  // ============== SEARCH ==============

  /** Tìm kiếm người dùng */
  searchUsers(
    keyword: string,
    limit: number = 20
  ): Observable<ResponseObject<FriendSummaryResponse[]>> {
    return this.http.get<ResponseObject<FriendSummaryResponse[]>>(
      `${this.api}/search?keyword=${encodeURIComponent(keyword)}&limit=${limit}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  // ============== RELATIONSHIP STATUS ==============

  /** Kiểm tra trạng thái quan hệ */
  getRelationshipStatus(user_id: number): Observable<ResponseObject<string>> {
    return this.http.get<ResponseObject<string>>(`${this.api}/status/${user_id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }
}
