import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {Observable} from 'rxjs';
import {HttpUtilService} from './http.util.service';
import {ResponseObject} from '../responses/response-object';
import {AdminDashboardStatsResponse} from '../responses/admin/admin-dashboard-stats-response';
import {environment} from '../environments/environment';
import {UserNewPerDayResponse} from '../responses/admin/user-new-per-day-response';
import {BoCauHoiStatusStatsResponse} from '../responses/admin/bo-cau-hoi-status-stats-response';

@Injectable({
  providedIn: 'root'
})
export class AdminService {

  private readonly api = `${environment.apiBaseUrl}/admin/dashboard`;

  constructor(
    private http: HttpClient,
    private httpUtil: HttpUtilService
  ) {
  }

  get_dashboard_stats(): Observable<ResponseObject<AdminDashboardStatsResponse>> {
    return this.http.get<ResponseObject<AdminDashboardStatsResponse>>(
      `${this.api}/stats`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }


  get_user_new_per_day(days: number) {
    return this.http.get<ResponseObject<UserNewPerDayResponse[]>>(
      `${this.api}/user-new-per-day?days=${days}`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }


  get_bo_cau_hoi_status_stats() {
    return this.http.get<ResponseObject<BoCauHoiStatusStatsResponse>>(
      `${this.api}/bo-cau-hoi-status-stats`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }


}
