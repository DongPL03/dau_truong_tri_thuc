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
import {BattleInviteDto} from '../dtos/tran-dau/battle-invite-dto';


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
    size: number = 5,
    loai_tran_dau?: 'CASUAL' | 'RANKED'
  ): Observable<ResponseObject<PageResponse<TranDauResponse>>> {
    const params: any = {page, size};
    if (loai_tran_dau) {
      params.loai_tran_dau = loai_tran_dau;
    }
    return this.http.get<ResponseObject<PageResponse<TranDauResponse>>>(
      `${this.api}/pending`,
      {
        headers: this.httpUtil.createAuthHeaders(),
        params
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
   * 🔹 Lấy toàn bộ lịch sử trận đấu (dành cho Admin)
   */
  getAllHistory(page = 0, limit = 10) {
    const params = {page, limit};
    return this.http.get<ResponseObject<PageResponse<LichSuTranDauResponse>>>(
      `${environment.apiBaseUrl}/tranDau/history/all`,
      {params, headers: this.httpUtil.createAuthHeaders()}
    );
  }


  /**
   *  🔹 Lấy lịch sử trận đấu của 1 user bất kỳ (dùng cho bảng xếp hạng)
   */
  getUserHistory(user_id: number, page = 0, limit = 10) {
    const params = {page, limit};
    return this.http.get<ResponseObject<PageResponse<LichSuTranDauResponse>>>(
      `${environment.apiBaseUrl}/tranDau/history/user/${user_id}`,
      {params, headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /**
   * 🔹 Lấy chi tiết lịch sử trận đấu theo lich_su_id (dành cho Admin)
   */
  getHistoryDetailAdmin(lich_su_id: number) {
    return this.http.get<ResponseObject<any>>(
      `${environment.apiBaseUrl}/tranDau/history/admin/${lich_su_id}`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /** Admin – tất cả câu trả lời của 1 người chơi trong trận */
  getPlayerAnswersAdmin(tran_dau_id: number, user_id: number) {
    const params = {tranDauId: tran_dau_id, userId: user_id};
    return this.http.get<ResponseObject<any>>(
      `${environment.apiBaseUrl}/tranDau/history/admin/player-answers`,
      {params, headers: this.httpUtil.createAuthHeaders()}
    );
  }

  /** Admin – tất cả câu trả lời của tất cả người chơi cho 1 câu hỏi trong trận */
  getQuestionAnswersAdmin(tran_dau_id: number, cau_hoi_id: number) {
    const params = {tranDauId: tran_dau_id, cauHoiId: cau_hoi_id};
    return this.http.get<ResponseObject<any>>(
      `${environment.apiBaseUrl}/tranDau/history/admin/question-answers`,
      {params, headers: this.httpUtil.createAuthHeaders()}
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

  inviteFriend(tran_dau_id: number, target_user_id: number):
    Observable<ResponseObject<any>> {

    const body: BattleInviteDto = {target_user_id};
    return this.http.post<ResponseObject<any>>(
      `${this.api}/${tran_dau_id}/invite-friend`,
      body,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }
}
