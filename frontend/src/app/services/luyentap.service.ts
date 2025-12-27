import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { BatDauLuyenTapRequest } from '../dtos/luyen-tap/bat_dau_luyen_tap-request';
import { TraLoiCauHoiPracticeDTO } from '../dtos/luyen-tap/tra-loi-cau-hoi-dto';
import { environment } from '../environments/environment';
import { BatDauLuyenTapResponse } from '../responses/luyentap/bat_dau_luyen_tap-response';
import { LichSuLuyenTapItem } from '../responses/luyentap/lich_su_luyen_tap-item';
import { SubmitLuyenTapResponse } from '../responses/luyentap/submit_luyen_tap-response';
import { TheGhiNhoResponse } from '../responses/luyentap/the_ghi_nho-response';
import { PageResponse } from '../responses/page-response';
import { ResponseObject } from '../responses/response-object';
import { HttpUtilService } from './http.util.service';

@Injectable({ providedIn: 'root' })
export class LuyenTapService {
  private readonly baseUrl = `${environment.apiBaseUrl}/luyenTap`;

  constructor(private http: HttpClient, private httpUtil: HttpUtilService) {}

  startPractice(dto: BatDauLuyenTapRequest) {
    return this.http.post<ResponseObject<BatDauLuyenTapResponse>>(`${this.baseUrl}/start`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  submitPractice(dto: TraLoiCauHoiPracticeDTO) {
    return this.http.post<ResponseObject<SubmitLuyenTapResponse>>(`${this.baseUrl}/submit`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /** Lịch sử luyện tập của user hiện tại (có filter khóa học / bộ câu hỏi) */
  getHistory(page = 0, size = 10, khoaHocId?: number | null, boCauHoiId?: number | null) {
    const params: any = { page, size };
    if (khoaHocId) params.khoaHocId = khoaHocId;
    if (boCauHoiId) params.boCauHoiId = boCauHoiId;

    return this.http.get<ResponseObject<PageResponse<LichSuLuyenTapItem>>>(
      `${this.baseUrl}/history`,
      {
        params,
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  /** Xem lại chi tiết một phiên (sau này dùng với KetQuaLuyenTapResponse) */
  getResult(phienId: number) {
    return this.http.get<ResponseObject<any>>(`${this.baseUrl}/${phienId}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  /** 🆕 Lấy danh sách thẻ ghi nhớ */
  getMemos(page = 0, size = 10) {
    const params = { page, size };
    return this.http.get<ResponseObject<PageResponse<TheGhiNhoResponse>>>(
      `${this.baseUrl}/memo/list`,
      {
        params,
        headers: this.httpUtil.createAuthHeaders(),
      }
    );
  }

  /** 🆕 Bắt đầu luyện tập từ thẻ ghi nhớ */
  startPracticeFromMemos(bo_cau_hoi_id: number) {
    return this.http.post<ResponseObject<BatDauLuyenTapResponse>>(
      `${this.baseUrl}/memo/start/${bo_cau_hoi_id}`,
      {},
      { headers: this.httpUtil.createAuthHeaders() }
    );
  }

  /** 🆕 Xoá một thẻ ghi nhớ */
  deleteMemo(memo_id: number) {
    return this.http.delete<ResponseObject<any>>(`${this.baseUrl}/memo/delete/${memo_id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }
}
