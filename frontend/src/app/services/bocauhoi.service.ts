import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {Observable} from 'rxjs';
import {environment} from '../environments/environment';
import {HttpUtilService} from './http.util.service';

import {ResponseObject} from '../responses/response-object';
import {PageResponse} from '../responses/page-response';
import {BoCauHoiResponse} from '../responses/bocauhoi/bocauhoi-response';

@Injectable({providedIn: 'root'})
export class BocauhoiService {
  private readonly api = `${environment.apiBaseUrl}/boCauHoi`;

  constructor(
    private http: HttpClient,
    private httpUtil: HttpUtilService
  ) {
  }

  /**
   * 🔹 Lấy danh sách bộ câu hỏi với params linh hoạt
   */
  getAll(
    keyword: string = '',
    chuDeId: number = 0,
    cheDoHienThi: string = '',
    trangThai: string = '',
    sortOrder: string = 'NEWEST',
    page: number = 0,
    limit: number = 10
  ): Observable<ResponseObject<PageResponse<BoCauHoiResponse>>> {
    const params = {
      keyword,
      chu_de_id: chuDeId.toString(),
      che_do_hien_thi: cheDoHienThi.toString(),
      trang_thai: trangThai.toString(),
      sort_order: sortOrder,
      page,
      limit
    };
    return this.http.get<ResponseObject<PageResponse<BoCauHoiResponse>>>(
      `${this.api}`,
      {
        headers: this.httpUtil.createAuthHeaders(),
        params,
      }
    );
  }

  /**
   * 🔹 Lấy danh sách bộ câu hỏi nổi bật (giới hạn)
   */
  getFeatured(limit: number = 3): Observable<ResponseObject<PageResponse<BoCauHoiResponse>>> {
    const params = {limit};
    return this.http.get<ResponseObject<PageResponse<BoCauHoiResponse>>>(
      `${this.api}`,
      {
        headers: this.httpUtil.createAuthHeaders(),
        params,
      }
    );
  }

  /**
   * 🔹 Chi tiết bộ câu hỏi
   */
  getById(id: number): Observable<ResponseObject<BoCauHoiResponse>> {
    return this.http.get<ResponseObject<BoCauHoiResponse>>(
      `${this.api}/${id}`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Tạo bộ câu hỏi mới
   */
  create(dto: any): Observable<ResponseObject<BoCauHoiResponse>> {
    return this.http.post<ResponseObject<BoCauHoiResponse>>(
      `${this.api}`,
      dto,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Cập nhật bộ câu hỏi
   */
  update(id: number, dto: any): Observable<ResponseObject<BoCauHoiResponse>> {
    return this.http.put<ResponseObject<BoCauHoiResponse>>(
      `${this.api}/${id}`,
      dto,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Xóa (soft delete)
   */
  delete(id: number): Observable<ResponseObject<null>> {
    return this.http.delete<ResponseObject<null>>(
      `${this.api}/${id}`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
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
      limit: 100
    };

    // 🔁 GỌI SANG /practice-sets
    return this.http.get<ResponseObject<PageResponse<BoCauHoiResponse>>>(
      `${this.api}/practice-sets`,
      {params}
    );
  }

  /** Danh sách bộ câu hỏi dùng cho thi đấu:
   *  chỉ lấy các bộ mà backend cho phép (official)
   */
  getBattleSets() {
    const params: any = {
      page: 0,
      limit: 100
    };

    return this.http.get<ResponseObject<PageResponse<BoCauHoiResponse>>>(
      `${this.api}/battle-sets`,
      {params}
    );
  }

  /** Gắn cờ Official cho 1 bộ câu hỏi (chỉ admin)
   * @param id
   */
  markOfficial(id: number) {
    return this.http.put<ResponseObject<BoCauHoiResponse>>(
      `${this.api}/${id}/mark-official`,
      {},
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

}
