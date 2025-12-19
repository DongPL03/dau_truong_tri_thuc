import {Injectable} from '@angular/core';
import {HttpClient} from '@angular/common/http';
import {HttpUtilService} from './http.util.service';
import {environment} from '../environments/environment';

@Injectable({providedIn: 'root'})
export class ThanhtichService {
  private readonly api = `${environment.apiBaseUrl}/achievements`;

  constructor(
    private http: HttpClient,
    private httpUtil: HttpUtilService
  ) {
  }

  getMyAchievements() {
    return this.http.get(
      `${this.api}/me`,
      {headers: this.httpUtil.createAuthHeaders()}
    );
  }

}
