import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {Observable} from 'rxjs';
import {environment} from '../environments/environment';
import {HttpUtilService} from './http.util.service';
import {ResponseObject} from '../responses/response-object';
import {FriendRequestDTO} from '../responses/banbe/friend_request_dto';
import {FriendRequestItemResponse} from '../responses/banbe/friend_request_item_response';
import {FriendSummaryResponse} from '../responses/banbe/friend_summary_response';


@Injectable({providedIn: 'root'})
export class FriendService {

  private readonly api = `${environment.apiBaseUrl}/friends`;

  constructor(
    private http: HttpClient,
    private httpUtil: HttpUtilService
  ) {
  }

  /** Gửi lời mời kết bạn */
  sendRequest(dto: FriendRequestDTO): Observable<ResponseObject<null>> {
    return this.http.post<ResponseObject<null>>(
      `${this.api}/request`,
      dto,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /** Chấp nhận lời mời */
  acceptRequest(request_id: number): Observable<ResponseObject<null>> {
    return this.http.post<ResponseObject<null>>(
      `${this.api}/requests/${request_id}/accept`,
      {},
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /** Từ chối lời mời */
  declineRequest(request_id: number): Observable<ResponseObject<null>> {
    return this.http.post<ResponseObject<null>>(
      `${this.api}/requests/${request_id}/decline`,
      {},
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /** Huỷ lời mời đã gửi */
  cancelRequest(request_id: number): Observable<ResponseObject<null>> {
    return this.http.post<ResponseObject<null>>(
      `${this.api}/requests/${request_id}/cancel`,
      {},
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /** Huỷ kết bạn */
  unfriend(friend_user_id: number): Observable<ResponseObject<null>> {
    return this.http.delete<ResponseObject<null>>(
      `${this.api}/${friend_user_id}`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /** Danh sách lời mời đến */
  getIncomingRequests(): Observable<ResponseObject<FriendRequestItemResponse[]>> {
    return this.http.get<ResponseObject<FriendRequestItemResponse[]>>(
      `${this.api}/requests/incoming`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /** Danh sách lời mời đã gửi */
  getOutgoingRequests(): Observable<ResponseObject<FriendRequestItemResponse[]>> {
    return this.http.get<ResponseObject<FriendRequestItemResponse[]>>(
      `${this.api}/requests/outgoing`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /** Danh sách bạn bè */
  getFriends(): Observable<ResponseObject<FriendSummaryResponse[]>> {
    return this.http.get<ResponseObject<FriendSummaryResponse[]>>(
      `${this.api}`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }
}
