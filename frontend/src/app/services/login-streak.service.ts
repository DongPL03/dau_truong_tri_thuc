import { HttpClient } from '@angular/common/http';
import { Injectable, inject } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from '../environments/environment';
import { LoginStreakResponse } from '../models/login-streak.model';
import { ResponseObject } from '../responses/response-object';

@Injectable({
  providedIn: 'root',
})
export class LoginStreakService {
  private http = inject(HttpClient);
  private apiUrl = `${environment.apiBaseUrl}/login-streak`;

  /**
   * Lấy thông tin chuỗi đăng nhập
   */
  getStreakInfo(): Observable<ResponseObject<LoginStreakResponse>> {
    return this.http.get<ResponseObject<LoginStreakResponse>>(this.apiUrl);
  }

  /**
   * Điểm danh và nhận thưởng
   */
  claimDailyReward(): Observable<ResponseObject<LoginStreakResponse>> {
    return this.http.post<ResponseObject<LoginStreakResponse>>(`${this.apiUrl}/claim`, {});
  }
}
