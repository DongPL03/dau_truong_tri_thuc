import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from '../environments/environment';
import { HttpUtilService } from './http.util.service';

import { BoCauHoiResponse } from '../responses/bocauhoi/bocauhoi-response';
import { UnlockBoCauHoiResponse } from '../responses/bocauhoi/unlock-bo-cau-hoi-response';
import { PageResponse } from '../responses/page-response';
import { ResponseObject } from '../responses/response-object';

@Injectable({ providedIn: 'root' })
export class BocauhoiService {
  private readonly api = `${environment.apiBaseUrl}/boCauHoi`;

  constructor(private http: HttpClient, private httpUtil: HttpUtilService) {}

  /**
   * 🔹 Lấy danh sách bộ câu hỏi với params linh hoạt
   */
  getAll(
    keyword: string = '',
    chuDeId: number = 0,
    cheDoHienThi: string = '',
    trangThai: string = '',
    loaiSuDung: string = '',
    muonTaoTraPhi?: boolean,
    nguoiTaoId: number = 0,
    sortOrder: string = 'NEWEST',
    page: number = 0,
    limit: number = 10,
    minRating?: number,
    maxRating?: number
  ): Observable<ResponseObject<PageResponse<BoCauHoiResponse>>> {
    const params: any = {
      keyword,
      chu_de_id: chuDeId.toString(),
      che_do_hien_thi: cheDoHienThi.toString(),
      trang_thai: trangThai.toString(),
      loai_su_dung: loaiSuDung.toString(),
      nguoi_tao_id: nguoiTaoId.toString(),
      sort_order: sortOrder,
      page,
      limit,
    };

    if (muonTaoTraPhi !== undefined && muonTaoTraPhi !== null) {
      params.muon_tao_tra_phi = muonTaoTraPhi.toString();
    }

    if (minRating !== undefined && minRating !== null) {
      params.min_rating = minRating.toString();
    }

    if (maxRating !== undefined && maxRating !== null) {
      params.max_rating = maxRating.toString();
    }

    return this.http.get<ResponseObject<PageResponse<BoCauHoiResponse>>>(`${this.api}`, {
      headers: this.httpUtil.createAuthHeaders(),
      params,
    });
  }

  /**
   * 🔹 Lấy danh sách bộ câu hỏi nổi bật (giới hạn)
   */
  getFeatured(limit: number = 3): Observable<ResponseObject<PageResponse<BoCauHoiResponse>>> {
    const params = { limit };
    return this.http.get<ResponseObject<PageResponse<BoCauHoiResponse>>>(`${this.api}`, {
      headers: this.httpUtil.createAuthHeaders(),
      params,
    });
  }

  /**
   * 🔹 Chi tiết bộ câu hỏi
   */
  getById(id: number): Observable<ResponseObject<BoCauHoiResponse>> {
    return this.http.get<ResponseObject<BoCauHoiResponse>>(`${this.api}/${id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * 🔹 Tạo bộ câu hỏi mới
   */
  create(dto: any): Observable<ResponseObject<BoCauHoiResponse>> {
    return this.http.post<ResponseObject<BoCauHoiResponse>>(`${this.api}`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * 🔹 Cập nhật bộ câu hỏi
   */
  update(id: number, dto: any): Observable<ResponseObject<BoCauHoiResponse>> {
    return this.http.put<ResponseObject<BoCauHoiResponse>>(`${this.api}/${id}`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * 🔹 Bulk approve nhiều bộ câu hỏi
   */
  bulkApprove(ids: number[]): Observable<ResponseObject<any>> {
    return this.http.post<ResponseObject<any>>(`${this.api}/bulk-approve`, ids, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * 🔹 Bulk reject nhiều bộ câu hỏi
   */
  bulkReject(ids: number[], lyDo: string): Observable<ResponseObject<any>> {
    return this.http.post<ResponseObject<any>>(
      `${this.api}/bulk-reject`,
      { ids, lyDo },
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /**
   * 🔹 Lấy thống kê bộ câu hỏi
   */
  getStatistics(): Observable<ResponseObject<any>> {
    return this.http.get<ResponseObject<any>>(`${this.api}/statistics`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * 🔹 Duplicate bộ câu hỏi (chỉ admin)
   */
  duplicate(id: number, loaiSuDung: string, purpose: string): Observable<ResponseObject<BoCauHoiResponse>> {
    const params = {
      loai_su_dung: loaiSuDung,
      purpose: purpose,
    };
    return this.http.post<ResponseObject<BoCauHoiResponse>>(
      `${this.api}/${id}/duplicate`,
      {},
      {
        headers: this.httpUtil.createAuthHeaders(),
        params: params
      }
    );
  }

  /**
   * 🔹 Xóa (soft delete)
   */
  delete(id: number): Observable<ResponseObject<null>> {
    return this.http.delete<ResponseObject<null>>(`${this.api}/${id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /** Danh sách bộ câu hỏi dùng cho luyện tập:
   *  chỉ lấy các bộ mà backend cho phép (public + của chính user)
   */
  getPracticeSets() {
    const params: any = {
      keyword: '',
      chu_de_id: 0,
      che_do_hien_thi: '',
      trang_thai: '',
      sort_order: 'NEWEST',
      page: 0,
      limit: 100,
    };

    // 🔁 GỌI SANG /practice-sets
    return this.http.get<ResponseObject<PageResponse<BoCauHoiResponse>>>(
      `${this.api}/practice-sets`,
      { params }
    );
  }

  /** Danh sách bộ câu hỏi dùng cho thi đấu:
   *  chỉ lấy các bộ mà backend cho phép (official)
   */
  getBattleSets() {
    const params: any = {
      page: 0,
      limit: 100,
    };

    return this.http.get<ResponseObject<PageResponse<BoCauHoiResponse>>>(
      `${this.api}/battle-sets`,
      { params }
    );
  }

  /**
   * ✅ Lấy danh sách bộ câu hỏi dùng cho trận đấu thường
   */
  getCasualBattleSets() {
    return this.http.get<ResponseObject<BoCauHoiResponse[]>>(`${this.api}/battle-sets/casual`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /**
   * ✅ Lấy danh sách bộ câu hỏi dùng cho trận đấu xếp hạng
   */
  getRankedBattleSets() {
    return this.http.get<ResponseObject<BoCauHoiResponse[]>>(`${this.api}/battle-sets/ranked`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /** Gắn cờ Official cho 1 bộ câu hỏi (chỉ admin)
   * @param id
   */
  markOfficial(id: number) {
    return this.http.put<ResponseObject<BoCauHoiResponse>>(
      `${this.api}/${id}/mark-official`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /** Gắn cờ Official cho 1 bộ câu hỏi (chỉ admin)
   * @param id
   */
  disMarkOfficial(id: number) {
    return this.http.put<ResponseObject<BoCauHoiResponse>>(
      `${this.api}/${id}/dis-mark-official`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /**
   * ✅ Admin duyệt bộ câu hỏi
   * */
  approveBoCauHoi(id: number): Observable<ResponseObject<BoCauHoiResponse>> {
    return this.http.put<ResponseObject<BoCauHoiResponse>>(
      `${this.api}/${id}/approve`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /**
   * ❌ Admin từ chối bộ câu hỏi với lý do
   * */
  rejectBoCauHoi(id: number, lyDoTuChoi: string): Observable<ResponseObject<BoCauHoiResponse>> {
    const body = { ly_do_tu_choi: lyDoTuChoi };
    return this.http.put<ResponseObject<BoCauHoiResponse>>(`${this.api}/${id}/reject`, body, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  unlock_bo_cau_hoi(id: number) {
    const url = `${this.api}/unlock/${id}`;
    return this.http.put<ResponseObject<UnlockBoCauHoiResponse>>(
      url,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }
}
