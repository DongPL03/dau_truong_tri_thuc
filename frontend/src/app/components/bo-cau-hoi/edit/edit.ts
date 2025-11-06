import {Component, OnInit, ViewChild} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule, NgForm} from '@angular/forms';
import Swal from 'sweetalert2';
import {Base} from '../../base/base';
import {BoCauHoiResponse} from '../../../responses/bocauhoi/bocauhoi-response';
import {ResponseObject} from '../../../responses/response-object';
import {ChuDe} from '../../../models/chude';

@Component({
  selector: 'app-bo-cau-hoi-edit',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './edit.html',
  styleUrl: './edit.scss'
})
export class BoCauHoiEdit extends Base implements OnInit {
  @ViewChild('editForm') editForm!: NgForm;
  boCauHoiId!: number;
  dto: any = {
    tieu_de: '',
    mo_ta: '',
    chu_de_id: 0,
    che_do_hien_thi: 'PRIVATE'
  };
  loading = false;
  saving = false;
  chuDes: ChuDe[] = [];

  ngOnInit(): void {
    this.boCauHoiId = Number(this.route.snapshot.paramMap.get('id'));
    this.fetchChuDes();
    this.fetchBoCauHoi();
  }

  fetchChuDes() {
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

  fetchBoCauHoi() {
    this.loading = true;
    this.bocauHoiService.getById(this.boCauHoiId).subscribe({
      next: (res: ResponseObject<BoCauHoiResponse>) => {
        const d = res.data!;
        console.log(d);
        this.dto = {
          tieu_de: d.tieu_de,
          mo_ta: d.mo_ta,
          chu_de_id: d.chu_de_id,
          che_do_hien_thi: d.che_do_hien_thi
        };
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải dữ liệu bộ câu hỏi', 'error').then(() => {
          this.router.navigateByUrl('/bo-cau-hoi/list').then(r => {
          });
        });
      }
    });
  }

  onSubmit(form: NgForm) {
    if (form.invalid) {
      Swal.fire('Cảnh báo', 'Vui lòng nhập đầy đủ thông tin', 'warning').then(r => {
      });
      return;
    }

    this.saving = true;
    this.bocauHoiService.update(this.boCauHoiId, this.dto).subscribe({
      next: (res) => {
        Swal.fire('Thành công', 'Cập nhật bộ câu hỏi thành công', 'success').then(() => {
          this.router.navigate(['/bo-cau-hoi/detail', this.boCauHoiId]).then(r => {
          });
        });
      },
      error: (err) => {
        Swal.fire('Lỗi', err.error?.message || 'Không thể cập nhật bộ câu hỏi', 'error').then(r => {
        });
      },
      complete: () => this.saving = false
    });
  }

  cancel() {
    this.router.navigate(['/bo-cau-hoi/detail', this.boCauHoiId]).then(r => {
    });
  }
}
