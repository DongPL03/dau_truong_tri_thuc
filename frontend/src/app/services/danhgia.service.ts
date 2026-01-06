import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import {
  CreateDanhGiaRequest,
  DanhGiaResponse,
  DanhGiaStatsResponse,
  PageResponse,
} from '../dtos/danhgia/danhgia.dto';
import { environment } from '../environments/environment';
import { HttpUtilService } from './http.util.service';

interface ApiResponse<T> {
  status: string;
  message: string;
  data: T;
}

@Injectable({
  providedIn: 'root',
})
export class DanhGiaService {
  private apiUrl = `${environment.apiBaseUrl}/danh-gia`;

  constructor(private http: HttpClient, private httpUtil: HttpUtilService) {}

  /**
   * Tạo hoặc cập nhật đánh giá
   */
  createOrUpdate(request: CreateDanhGiaRequest): Observable<ApiResponse<DanhGiaResponse>> {
    return this.http.post<ApiResponse<DanhGiaResponse>>(this.apiUrl, request, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * Lấy đánh giá của tôi cho một đối tượng
   */
  getMyRating(
    loaiDoiTuong: string,
    doiTuongId: number
  ): Observable<ApiResponse<DanhGiaResponse | null>> {
    return this.http.get<ApiResponse<DanhGiaResponse | null>>(
      `${this.apiUrl}/my/${loaiDoiTuong}/${doiTuongId}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /**
   * Lấy danh sách đánh giá của đối tượng (phân trang)
   */
  getRatings(
    loaiDoiTuong: string,
    doiTuongId: number,
    page: number = 0,
    size: number = 10
  ): Observable<ApiResponse<PageResponse<DanhGiaResponse>>> {
    return this.http.get<ApiResponse<PageResponse<DanhGiaResponse>>>(
      `${this.apiUrl}/${loaiDoiTuong}/${doiTuongId}?page=${page}&size=${size}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /**
   * Lấy thống kê đánh giá
   */
  getStats(
    loaiDoiTuong: string,
    doiTuongId: number
  ): Observable<ApiResponse<DanhGiaStatsResponse>> {
    return this.http.get<ApiResponse<DanhGiaStatsResponse>>(
      `${this.apiUrl}/stats/${loaiDoiTuong}/${doiTuongId}`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /**
   * Xóa đánh giá
   */
  delete(id: number): Observable<ApiResponse<void>> {
    return this.http.delete<ApiResponse<void>>(`${this.apiUrl}/${id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }
}
