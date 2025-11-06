import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {Observable} from 'rxjs';
import {environment} from '../environments/environment';
import {HttpUtilService} from './http.util.service';

import {ResponseObject} from '../responses/response-object';
import {PageResponse} from '../responses/page-response';
import {TrandauResponse} from '../responses/trandau/trandau-response';

@Injectable({providedIn: 'root'})
export class TrandauService {
  private readonly api = `${environment.apiBaseUrl}/tranDau`;

  constructor(
    private http: HttpClient,
    private httpUtil: HttpUtilService
  ) {
  }

  /**
   * 🔹 Lấy danh sách phòng đang chờ (pending)
   */
  getPendingBattles(
    page: number = 0,
    size: number = 5
  ): Observable<ResponseObject<PageResponse<TrandauResponse>>> {
    const params = {page, size};
    return this.http.get<ResponseObject<PageResponse<TrandauResponse>>>(
      `${this.api}/pending`,
      {
        headers: this.httpUtil.createAuthHeaders(),
        params,
      }
    );
  }

  /**
   * 🔹 Chi tiết 1 trận đấu
   */
  getBattleDetail(id: number): Observable<ResponseObject<TrandauResponse>> {
    return this.http.get<ResponseObject<TrandauResponse>>(
      `${this.api}/${id}`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Tạo phòng đấu
   */
  createBattle(payload: any): Observable<ResponseObject<TrandauResponse>> {
    return this.http.post<ResponseObject<TrandauResponse>>(
      `${this.api}/create`,
      payload,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Tham gia phòng đấu
   */
  joinBattle(payload: any): Observable<ResponseObject<TrandauResponse>> {
    return this.http.post<ResponseObject<TrandauResponse>>(
      `${this.api}/join`,
      payload,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Rời phòng đấu
   */
  leaveBattle(payload: any): Observable<ResponseObject<null>> {
    return this.http.post<ResponseObject<null>>(
      `${this.api}/leave`,
      payload,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Bắt đầu trận đấu
   */
  startBattle(id: number): Observable<ResponseObject<any>> {
    return this.http.put<ResponseObject<any>>(
      `${this.api}/start/${id}`,
      {},
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Nộp đáp án
   */
  submitAnswer(payload: any): Observable<ResponseObject<any>> {
    return this.http.post<ResponseObject<any>>(
      `${this.api}/submit-answer`,
      payload,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Kết thúc trận đấu
   */
  finishBattle(id: number): Observable<ResponseObject<any>> {
    return this.http.put<ResponseObject<any>>(
      `${this.api}/finish/${id}`,
      {},
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }
}
