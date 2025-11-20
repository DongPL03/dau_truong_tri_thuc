import {Injectable} from '@angular/core';
import {HttpClient, HttpHeaders} from '@angular/common/http';
import {Observable} from 'rxjs';

import {TokenService} from './token.service';
import {environment} from '../environments/environment';
import {HttpUtilService} from './http.util.service';
import {CauHoiDTO} from '../dtos/cau-hoi/cauhoi-dto';
import {ResponseObject} from '../responses/response-object';
import {CauHoiResponse} from '../responses/cauhoi/cauhoi-response';
import {PageResponse} from '../responses/page-response';

@Injectable({providedIn: 'root'})
export class CauHoiService {
  private readonly api = `${environment.apiBaseUrl}/cauHoi`;

  constructor(
    private http: HttpClient,
    private httpUtil: HttpUtilService,
    private tokenService: TokenService
  ) {
  }

  getByBoCauHoi(
    boCauHoiId: number,
    page: number = 0,
    limit: number = 10
  ): Observable<ResponseObject<PageResponse<CauHoiResponse>>> {
    const params = {page, limit};
    return this.http.get<ResponseObject<PageResponse<CauHoiResponse>>>(
      `${this.api}/bo/${boCauHoiId}`,
      {params}
    );
  }


  create(dto: CauHoiDTO): Observable<ResponseObject<CauHoiResponse>> {
    return this.http.post<ResponseObject<CauHoiResponse>>(
      this.api,
      dto,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  uploadMedia(cauHoiId: number, file: File, loaiNoiDung: string): Observable<ResponseObject<string>> {
    const token = this.tokenService.getAccessToken();
    const formData = new FormData();
    formData.append('file', file);
    formData.append('loaiNoiDung', loaiNoiDung);
    const headers = new HttpHeaders({
      Authorization: `Bearer ${token ?? ''}`
    });
    return this.http.post<ResponseObject<string>>(`${this.api}/upload/${cauHoiId}`, formData, {headers});
  }

  getById(id: number): Observable<ResponseObject<CauHoiResponse>> {
    return this.http.get<ResponseObject<CauHoiResponse>>(`${this.api}/${id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  update(id: number, dto: any): Observable<ResponseObject<CauHoiResponse>> {
    return this.http.put<ResponseObject<CauHoiResponse>>(`${this.api}/${id}`, dto, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }

  delete(id: number): Observable<ResponseObject> {
    return this.http.delete<ResponseObject>(`${this.api}/${id}`, {
      headers: this.httpUtil.createAuthHeaders(),
    });
  }
}
