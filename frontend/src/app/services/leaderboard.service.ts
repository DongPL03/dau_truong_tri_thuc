import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';

import {environment} from '../environments/environment';
import {HttpUtilService} from './http.util.service';
import {ResponseObject} from '../responses/response-object';
import {PageResponse} from '../responses/page-response';
import {LeaderboardEntryResponse} from '../responses/bangxephang/leaderboard-entry-response';

@Injectable({providedIn: 'root'})
export class LeaderboardService {
  private readonly api = `${environment.apiBaseUrl}/leaderboard`;

  constructor(
    private http: HttpClient,
    private httpUtil: HttpUtilService
  ) {
  }

  /**
   * Lấy bảng xếp hạng toàn cục
   */
  getGlobal(
    page: number = 0,
    limit: number = 20,
    time_range: string = 'ALL',
    chu_de_id?: number,
    bo_cau_hoi_id?: number,
    // friend_only: boolean = false
  ) {
    // const params: any = { page, limit, time_range, friend_only };
    const params: any = {page, limit, time_range};

    if (chu_de_id) params.chu_de_id = chu_de_id;
    if (bo_cau_hoi_id) params.bo_cau_hoi_id = bo_cau_hoi_id;

    return this.http.get<ResponseObject<PageResponse<LeaderboardEntryResponse>>>(
      `${this.api}/global`,
      {params, headers: this.httpUtil.createAuthHeaders()}
    );
  }

}
