import {Component, OnInit, ViewChild} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule, NgForm} from '@angular/forms';
import Swal from 'sweetalert2';
import {Base} from '../../base/base';
import {ResponseObject} from '../../../responses/response-object';
import {BoCauHoiResponse} from '../../../responses/bocauhoi/bocauhoi-response';
import {ChuDe} from '../../../models/chude';

@Component({
  selector: 'app-bo-cau-hoi-tao-moi-bo-cau-hoi',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './tao-moi-bo-cau-hoi.html',
  styleUrl: './tao-moi-bo-cau-hoi.scss'
})
export class BoCauHoiCreate extends Base implements OnInit {
  @ViewChild('createForm') createForm!: NgForm;

  chuDes: ChuDe[] = [];
  creating = false;

  model = {
    tieu_de: '',
    mo_ta: '',
    chu_de_id: 0,
    che_do_hien_thi: 'PUBLIC'
  };


  ngOnInit(): void {
    this.loadChuDe();
  }

  /** Gọi API lấy danh sách chủ đề */
  loadChuDe() {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<any>) => {
        this.chuDes = res.data || [];
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách chủ đề', 'error').then(r => {
        });
      }
    });
  }

  /** Submit form tạo bộ câu hỏi */
  onSubmit(form: NgForm) {
    if (form.invalid || this.model.chu_de_id === 0) {
      Swal.fire('Cảnh báo', 'Vui lòng điền đủ thông tin bắt buộc', 'warning').then(r => {
      });
      return;
    }

    this.creating = true;

    this.bocauHoiService.create(this.model).subscribe({
      next: (res: ResponseObject<BoCauHoiResponse>) => {
        Swal.fire('Thành công', 'Tạo bộ câu hỏi thành công!', 'success').then(() => {
          this.router.navigateByUrl('/bo-cau-hoi/danh-sach-bo-cau-hoi').then(r => {
          });
        });
      },
      error: (err) => {
        Swal.fire('Lỗi', err.error?.message || 'Không thể tạo bộ câu hỏi', 'error').then(r => {
        });
      },
      complete: () => (this.creating = false)
    });
  }

  cancel() {
    this.router.navigateByUrl('/bo-cau-hoi/danh-sach-bo-cau-hoi').then(r => {
    });
  }
}
