import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {Observable} from 'rxjs';
import {environment} from '../environments/environment';
import {HttpUtilService} from './http.util.service';

import {ResponseObject} from '../responses/response-object';
import {PageResponse} from '../responses/page-response';
import {TranDauResponse} from '../responses/trandau/trandau-response';
import {ThamGiaTranDauDTO} from '../dtos/tran-dau/thamgiatrandau-dto';
import {TaoTranDauDTO} from '../dtos/tran-dau/taotran-dto';
import {RoiTranDauDTO} from '../dtos/tran-dau/roitran-dto';
import {SubmitAnswerDTO} from '../dtos/tran-dau/submitanswer-dto';
import {SyncStateResponse} from '../responses/trandau/syncstate-response';
import {LichSuTranDauResponse} from '../responses/trandau/lichsutrandau';
import {LichSuTranDauDetailResponse} from '../responses/trandau/lich-su-tran-dau-detail-response';
import {GuiChatDTO} from '../dtos/tran-dau/guichat-dto';


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
  ): Observable<ResponseObject<PageResponse<TranDauResponse>>> {
    const params = {page, size};
    return this.http.get<ResponseObject<PageResponse<TranDauResponse>>>(
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
  getBattleDetail(id: number): Observable<ResponseObject<TranDauResponse>> {
    return this.http.get<ResponseObject<TranDauResponse>>(
      `${this.api}/${id}`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Tạo phòng đấu
   */
  createBattle(dto: TaoTranDauDTO): Observable<ResponseObject<TranDauResponse>> {
    return this.http.post<ResponseObject<TranDauResponse>>(
      `${this.api}/create`,
      dto,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Tham gia phòng đấu
   */
  joinBattle(dto: ThamGiaTranDauDTO): Observable<ResponseObject<TranDauResponse>> {
    return this.http.post<ResponseObject<TranDauResponse>>(
      `${this.api}/join`,
      dto,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Rời phòng đấu
   */
  leaveBattle(dto: RoiTranDauDTO): Observable<ResponseObject<void>> {
    return this.http.post<ResponseObject<null>>(
      `${this.api}/leave`,
      dto,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  sync(id: number): Observable<ResponseObject<SyncStateResponse>> {
    return this.http.get<ResponseObject<SyncStateResponse>>(
      `${this.api}/sync/${id}`,
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
  submitAnswer(dto: SubmitAnswerDTO): Observable<ResponseObject<any>> {
    return this.http.post<ResponseObject<any>>(
      `${this.api}/submit-answer`,
      dto,
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

  /**
   *  🔹 Lấy lịch sử trận đấu của tôi
   */
  getMyHistory(page = 0, limit = 10) {
    const params = {page, limit};
    return this.http.get<ResponseObject<PageResponse<LichSuTranDauResponse>>>(
      `${environment.apiBaseUrl}/tranDau/history/my`,
      {params, headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   *  🔹 Lấy chi tiết lịch sử trận đấu của tôi
   */
  getMyHistoryDetail(tran_dau_id: number) {
    return this.http.get<ResponseObject<LichSuTranDauDetailResponse>>(
      `${environment.apiBaseUrl}/tranDau/history/my/${tran_dau_id}`
      , {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Gửi chat trong trận đấu
   * @param dto
   */
  sendChat(dto: GuiChatDTO) {
    return this.http.post<ResponseObject<any>>(
      `${environment.apiBaseUrl}/tranDau/chat`,
      dto
    );
  }
}
