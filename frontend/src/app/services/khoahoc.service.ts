import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from '../environments/environment';
import { CourseStatResponse } from '../responses/khoahoc/course-stat-response';
import { KhoaHoiDetailResponse } from '../responses/khoahoc/khoa-hoi-detail-response';
import { KhoaHoiResponse } from '../responses/khoahoc/khoa-hoi-response';
import { PageResponse } from '../responses/page-response';
import { ResponseObject } from '../responses/response-object';
import { HttpUtilService } from './http.util.service';

@Injectable({ providedIn: 'root' })
export class KhoahocService {
  private readonly api = `${environment.apiBaseUrl}/khoa-hoc`;

  constructor(private http: HttpClient, private httpUtil: HttpUtilService) {}

  /**
   * Lấy danh sách khóa học
   */
  getAll(
    keyword: string = '',
    chuDeId: number = 0,
    trangThai: string = '',
    sortOrder: string = 'NEWEST',
    page: number = 0,
    limit: number = 10,
    minRating?: number,
    maxRating?: number
  ): Observable<ResponseObject<PageResponse<KhoaHoiResponse>>> {
    let params: any = {
      keyword,
      chu_de_id: chuDeId.toString(),
      trang_thai: trangThai,
      sort_order: sortOrder,
      page: page.toString(),
      limit: limit.toString(),
    };
    if (minRating !== undefined) {
      params.min_rating = minRating.toString();
    }
    if (maxRating !== undefined) {
      params.max_rating = maxRating.toString();
    }
    return this.http.get<ResponseObject<PageResponse<KhoaHoiResponse>>>(`${this.api}`, {
      headers: this.httpUtil.createAuthHeaders(),
      params,
    });
  }

  /**
   * Lấy chi tiết khóa học
   */
  getById(id: number): Observable<ResponseObject<KhoaHoiDetailResponse>> {
    return this.http.get<ResponseObject<KhoaHoiDetailResponse>>(`${this.api}/${id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * Tạo khóa học mới (chỉ admin)
   */
  create(dto: any): Observable<ResponseObject<KhoaHoiResponse>> {
    return this.http.post<ResponseObject<KhoaHoiResponse>>(`${this.api}`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * Cập nhật khóa học (admin hoặc owner)
   */
  update(id: number, dto: any): Observable<ResponseObject<KhoaHoiResponse>> {
    return this.http.put<ResponseObject<KhoaHoiResponse>>(`${this.api}/${id}`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * Xóa cứng khóa học (admin hoặc owner)
   */
  delete(id: number): Observable<ResponseObject<null>> {
    return this.http.delete<ResponseObject<null>>(`${this.api}/${id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * Xóa mềm khóa học (admin hoặc owner)
   */
  softDelete(id: number): Observable<ResponseObject<null>> {
    return this.http.delete<ResponseObject<null>>(`${this.api}/${id}/soft`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * Unlock khóa học bằng vàng
   */
  unlockKhoaHoc(id: number): Observable<ResponseObject<any>> {
    return this.http.put<ResponseObject<any>>(
      `${this.api}/${id}/unlock`,
      {},
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  /**
   * Lấy phân tích học tập cho 1 khóa học (nếu đã có)
   */
  getPhanTichKhoaHoc(id: number): Observable<ResponseObject<any>> {
    return this.http.get<ResponseObject<any>>(
      `${environment.apiBaseUrl}/phan-tich-hoc-tap/khoa-hoc/${id}`,
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  /**
   * Thực hiện phân tích mạnh/yếu cho 1 khóa học và trả về kết quả
   */
  phanTichKhoaHoc(id: number): Observable<ResponseObject<any>> {
    return this.http.post<ResponseObject<any>>(
      `${environment.apiBaseUrl}/phan-tich-hoc-tap/khoa-hoc/${id}`,
      {},
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  /**
   * Thống kê tổng quan các khóa học của user
   */
  getCourseStats(): Observable<ResponseObject<CourseStatResponse>> {
    return this.http.get<ResponseObject<CourseStatResponse>>(
      `${environment.apiBaseUrl}/stats/courses/me`,
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /**
   * Thêm bộ câu hỏi vào khóa học
   */
  addBoCauHoiToKhoaHoc(
    khoaHocId: number,
    dto: { bo_cau_hoi_id: number; thu_tu: number; is_bat_buoc?: boolean; diem_toi_thieu?: number }
  ): Observable<ResponseObject<any>> {
    return this.http.post<ResponseObject<any>>(`${this.api}/${khoaHocId}/bo-cau-hoi`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * Cập nhật bộ câu hỏi trong khóa học
   */
  updateBoCauHoiInKhoaHoc(
    khoaHocId: number,
    boCauHoiId: number,
    dto: { thu_tu?: number; is_bat_buoc?: boolean; diem_toi_thieu?: number }
  ): Observable<ResponseObject<any>> {
    return this.http.put<ResponseObject<any>>(
      `${this.api}/${khoaHocId}/bo-cau-hoi/${boCauHoiId}`,
      dto,
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  /**
   * Xóa bộ câu hỏi khỏi khóa học
   */
  removeBoCauHoiFromKhoaHoc(
    khoaHocId: number,
    boCauHoiId: number
  ): Observable<ResponseObject<null>> {
    return this.http.delete<ResponseObject<null>>(
      `${this.api}/${khoaHocId}/bo-cau-hoi/${boCauHoiId}`,
      {
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }
}
