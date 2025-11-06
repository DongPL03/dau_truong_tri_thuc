import {inject} from '@angular/core';
import {ActivatedRoute, Router} from '@angular/router';
import {DOCUMENT, Location} from '@angular/common';
import {UserService} from '../../services/user.service';
import {TokenService} from '../../services/token.service';
import {TrandauService} from '../../services/trandau.service';
import {BocauhoiService} from '../../services/bocauhoi.service';
import {HttpUtilService} from '../../services/http.util.service';
import {ChudeService} from '../../services/chude.service';
import {CauHoiService} from '../../services/cauhoi.service';

export class Base {
  router: Router = inject(Router);
  route: ActivatedRoute = inject(ActivatedRoute);
  userService: UserService = inject(UserService);
  tokenService: TokenService = inject(TokenService);
  tranDauService: TrandauService = inject(TrandauService);
  bocauHoiService: BocauhoiService = inject(BocauhoiService);
  httpUtilService: HttpUtilService = inject(HttpUtilService);
  chuDeService: ChudeService = inject(ChudeService);
  cauHoiService: CauHoiService = inject(CauHoiService);
  document: Document = inject(DOCUMENT);
  location: Location = inject(Location);
}
