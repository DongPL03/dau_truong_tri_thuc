import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {Observable, Subject} from 'rxjs';
import {environment} from '../environments/environment';
import {HttpUtilService} from './http.util.service';
import {ResponseObject} from '../responses/response-object';
import {PageResponse} from '../responses/page-response';
import {NotificationResponse} from '../responses/notification/notification-response';

@Injectable({providedIn: 'root'})
export class NotificationService {

  private readonly api = `${environment.apiBaseUrl}/notifications`;

  private _realtime$ = new Subject<NotificationResponse>();
  realtime$ = this._realtime$.asObservable();

  constructor(
    private http: HttpClient,
    private httpUtil: HttpUtilService
  ) {
  }

  getMy(page = 0, limit = 10):
    Observable<ResponseObject<PageResponse<NotificationResponse>>> {
    const params = {page, limit};
    return this.http.get<ResponseObject<PageResponse<NotificationResponse>>>(
      `${this.api}/my`,
      {
        params,
        headers: this.httpUtil.createAuthHeaders()
      }
    );
  }

  getUnreadCount(): Observable<ResponseObject<number>> {
    return this.http.get<ResponseObject<number>>(
      `${this.api}/unread-count`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  markAsRead(id: number): Observable<ResponseObject<void>> {
    return this.http.put<ResponseObject<void>>(
      `${this.api}/${id}/read`,
      {},
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  markAllAsRead(): Observable<ResponseObject<void>> {
    return this.http.put<ResponseObject<void>>(
      `${this.api}/read-all`,
      {},
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  pushRealtime(notification: NotificationResponse) {
    this._realtime$.next(notification);
  }
}
