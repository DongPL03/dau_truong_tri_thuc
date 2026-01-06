// src/app/services/admin-stats.service.ts

import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

import { environment } from '../environments/environment';
import { ResponseObject } from '../responses/response-object';
import { AdminSummaryStatsResponse } from '../responses/thongke/admin-summary-stats-response';
import { DateCountResponse } from '../responses/thongke/date-count-response';
import { RatingOverviewStatsResponse } from '../responses/thongke/rating-stats-response';
import { TopBoCauHoiStatsResponse } from '../responses/thongke/top-bo-cau-hoi-stats-response';
import { TopPlayerStatsResponse } from '../responses/thongke/top-player-stats-response';
import { HttpUtilService } from './http.util.service';

@Injectable({ providedIn: 'root' })
export class AdminStatsService {
  private readonly api = `${environment.apiBaseUrl}/admin/stats`;

  constructor(private http: HttpClient, private http_util: HttpUtilService) {}

  /** 🔹 Thống kê tổng quan (cards KPI trên dashboard) */
  get_summary(): Observable<ResponseObject<AdminSummaryStatsResponse>> {
    return this.http.get<ResponseObject<AdminSummaryStatsResponse>>(`${this.api}/summary`, {
      headers: this.http_util.createAuthHeaders(),
    });
  }

  /** 🔹 Số trận theo ngày trong N ngày gần đây (line chart) */
  get_battles_by_day(days: number = 7): Observable<ResponseObject<DateCountResponse[]>> {
    return this.http.get<ResponseObject<DateCountResponse[]>>(`${this.api}/battles-by-day`, {
      headers: this.http_util.createAuthHeaders(),
      params: { days },
    });
  }

  /** 🔹 Top bộ câu hỏi được dùng nhiều nhất */
  get_top_bo_cau_hoi(limit: number = 5): Observable<ResponseObject<TopBoCauHoiStatsResponse[]>> {
    return this.http.get<ResponseObject<TopBoCauHoiStatsResponse[]>>(`${this.api}/top-bo-cau-hoi`, {
      headers: this.http_util.createAuthHeaders(),
      params: { limit },
    });
  }

  /** 🔹 Top người chơi (theo điểm tích lũy) */
  get_top_players(limit: number = 10): Observable<ResponseObject<TopPlayerStatsResponse[]>> {
    return this.http.get<ResponseObject<TopPlayerStatsResponse[]>>(`${this.api}/top-players`, {
      headers: this.http_util.createAuthHeaders(),
      params: { limit },
    });
  }

  /** 🔹 Thống kê đánh giá (rating overview) */
  get_rating_stats(limit: number = 5): Observable<ResponseObject<RatingOverviewStatsResponse>> {
    return this.http.get<ResponseObject<RatingOverviewStatsResponse>>(`${this.api}/rating-stats`, {
      headers: this.http_util.createAuthHeaders(),
      params: { limit },
    });
  }
}
