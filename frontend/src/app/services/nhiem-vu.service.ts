import { HttpClient } from '@angular/common/http';
import { Injectable, inject } from '@angular/core';
import { Observable } from 'rxjs';
import { map } from 'rxjs/operators';
import { environment } from '../environments/environment';
import { NhanThuongNhiemVuResponse, NhiemVuResponse } from '../models/nhiem-vu.model';
import { ResponseObject } from '../responses/response-object';

@Injectable({
  providedIn: 'root',
})
export class NhiemVuService {
  private http = inject(HttpClient);
  private apiUrl = `${environment.apiBaseUrl}/quests`;

  /**
   * Lấy danh sách nhiệm vụ (daily + weekly)
   */
  getQuests(): Observable<NhiemVuResponse> {
    return this.http
      .get<ResponseObject<NhiemVuResponse>>(this.apiUrl)
      .pipe(map((res) => res.data!));
  }

  /**
   * Nhận thưởng một nhiệm vụ
   */
  claimReward(questCode: string): Observable<NhanThuongNhiemVuResponse> {
    return this.http
      .post<ResponseObject<NhanThuongNhiemVuResponse>>(`${this.apiUrl}/claim/${questCode}`, {})
      .pipe(map((res) => res.data!));
  }

  /**
   * Nhận tất cả thưởng nhiệm vụ đã hoàn thành
   */
  claimAllRewards(): Observable<NhanThuongNhiemVuResponse> {
    return this.http
      .post<ResponseObject<NhanThuongNhiemVuResponse>>(`${this.apiUrl}/claim-all`, {})
      .pipe(map((res) => res.data!));
  }
}
