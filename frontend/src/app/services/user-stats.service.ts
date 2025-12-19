import {Injectable} from '@angular/core';
import {environment} from '../environments/environment';
import {HttpClient} from '@angular/common/http';
import {HttpUtilService} from './http.util.service';

@Injectable({providedIn: 'root'})
export class UserStatsService {
  private readonly api = `${environment.apiBaseUrl}/stats/topics`;

  constructor(
    private http: HttpClient,
    private http_util: HttpUtilService
  ) {}
  getMyTopicStats() {
    return this.http.get(
      `${this.api}/me`,
      {headers: this.http_util.createAuthHeaders()}
    );
  }
}
